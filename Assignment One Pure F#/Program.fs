
//This program reads a series of evolution commands from a file, runs these, and prints the resulting data to .fa and .homolog files, in their respecive formats.
//Data is stored as a Map<int, Map<(int*int), gene>>. The outer map holds what is essentially each species, which in turn hold each gene.
//The (int*int) key holds the current ID of the gene, specID being first and GeneID second.

//--------------------------------------------------------------------------------------//

//Holds the species and int that a 'segment' of DNA originated from,
//as well as where this element starts and ends, relative to that original string.
type origin = {species:int; gene:int; startPos:int; endPos:int}

//Holds the origins of every segment of its DNA, as well as the DNA itself.
type gene = {origins:(origin)list; DNA:string}

[<EntryPoint>]
let main argv = 
    let filename = argv.[0]


    let readLines =  System.IO.File.ReadLines(filename)
    use write = new System.IO.StreamWriter(filename + ".fa", false) // Write in FASTA format.
    use writeHom = new System.IO.StreamWriter(filename + ".homologs", false) // Write in homolog format.

    let toInt X = System.Int32.Parse X

    //Current length of an origin.
    let len x = 
        x.endPos - x.startPos + 1

    //Active pattern to match a string against a given prefix. There's probably an easier way to do this, but hey, it works.
    //Credit to the person who posted it on StackOverflow at some point.
    let (|Prefix|_|) (p:string) (s:string) =
        if s.StartsWith(p) then
            Some(s.Substring(p.Length))
        else
            None

 

    //Recursively matches each element of the origin list. If it ends before the index, discard it. If it starts afterwards, add it to the return list.
    //Index tracks where to split, updating to account for already processed origins.

    //'currentPoint' tracks the start of the currently processing string. It should always be called at 0 initially.
    //Surely there's a better way to handle that, no?
    let rec getSplitRight originList index currentPoint = 
        let splitOrigin origin index =  
            {species = origin.species; gene = origin.gene; startPos = index+origin.startPos; endPos = origin.endPos} //Take an origin and give a new one that starts at the given index.
        match originList with
        | [] -> []
        | head::tail -> match ((len head) + currentPoint > index) with
            | true -> match (currentPoint < index) with
                | false -> head :: getSplitRight tail (index - len head) (currentPoint + len head)
                | true -> splitOrigin head index :: getSplitRight tail (index - len head) (currentPoint + len head)
            | false -> getSplitRight tail (index - len head) (currentPoint) 

    //Same thing as getSplitRight, but this time it takes the origins to the left of the index.
    let rec getSplitLeft originList index currentPoint = 
        let splitOrigin origin index = 
            {species = origin.species; gene = origin.gene; startPos = origin.startPos; endPos = origin.startPos + index-1}
        match originList with
        | [] -> []
        | head::tail -> match (currentPoint < index) with
            | true -> match ((len head) + currentPoint < index) with
                | true -> head :: getSplitLeft tail (index - len head) (currentPoint) 
                | false -> splitOrigin head index :: getSplitLeft tail (index - len head) (currentPoint + len head) 
            | false -> getSplitLeft tail (index - len head) (currentPoint) 

//--------------------------------------------------------------------------------------//
    //Takes the elements for a gene, creates a gene, returns a new map with the gene added.
    let runCreate specID geneID newDNA world =
        let newGene = {origins = [{species = specID; gene = geneID; startPos = 0; endPos = (newDNA |> String.length)-1}]; DNA = newDNA}
        let newSpec = Map.ofList[(specID,geneID), newGene]

        //If the species already exists, no need to add it.
        match world |> Map.containsKey specID with 
        |false -> world.Add(specID, newSpec)
        |true -> world.Add(specID, (world.[specID].Add((specID,geneID), newGene)))
 //------------------------------------------------------------------//
    //Iterate over the characters in a string. When the index is reached, the character is replaced by newNuc.
    let snipGene index gene newNuc = String.mapi (fun i c -> if i = index then newNuc else c) gene

    //Snips the gene, creates the necessary elements, gives a new map with the new gene added.
    let runSnip specID geneID index newNuc world =
        let currentSpec = world |> Map.find specID
        let currentGene = currentSpec |> Map.find (specID, geneID)

        let newDNA = (snipGene index currentGene.DNA newNuc)
        let newGene = {origins = currentGene.origins; DNA = newDNA}
        let newSpec = currentSpec.Add((specID, geneID), newGene)

        world.Add (specID, newSpec)
//--------------------------------------------------------------------------------------------//
    let runInsert specID geneID index dnaSeg world =
        let currentSpecies = world |> Map.find specID
        let currentGene = currentSpecies |> Map.find (specID,geneID)

        let newDNA = currentGene.DNA.Insert(index, dnaSeg)
        //Look I know this *10 thing seems super hacky but I can't think of a better solution in time.
        //It's to distinguish the insertion event from that of the original creation. Think of it as 'clever and inventive' instead, okay?
        let newOrigin = {species = specID; gene = geneID*10; startPos = 0; endPos = dnaSeg.Length - 1}

        //Split the origins at the point the new DNA is to be inserted.
        let leftOrigins = getSplitLeft currentGene.origins index 0
        let rightOrigins = getSplitRight currentGene.origins (index) 0

        let newGene = {origins = leftOrigins@[newOrigin]@rightOrigins; DNA = newDNA}

        world.Add(specID, (world.[specID].Add((specID,geneID), newGene)))
//---------------------------------------------------------------------------------//
    let runDelete specID geneID index length world =
        let currentSpecies = world |> Map.find specID
        let currentGene = currentSpecies |> Map.find (specID, geneID)
        let leftOrigins = getSplitLeft currentGene.origins index 0
        let rightOrigins = getSplitRight currentGene.origins (index + length) 0 //Gets the origins to the right of the DNA being deleted.

        let newDNA = currentGene.DNA.Remove(index, length)
        let newGene = {origins = leftOrigins@rightOrigins; DNA = newDNA}

        world.Add(specID, (world.[specID].Add((specID, geneID), newGene))) //Origins to the left and right of the deleted DNA are concatenated.
 //-------------------------------------------------------------------------------------------//           
    let runDuplicate specID newGeneID oldGeneID world =
        let oldSpecies = world |> Map.find specID
        let oldGene = oldSpecies |> Map.find (specID, oldGeneID)

        world.Add(specID, (world.[specID].Add((specID, newGeneID), oldGene)))
//--------------------------------------------------------------------------------//
    let runLoss specID geneID world =
        let species = world |> Map.find specID
        let geneID = (specID, geneID)
        let newSpecies = species |> Map.remove(geneID)

        world.Add(specID, newSpecies)
//-------------------------------------------------------------------------------//
    let runFission specID newGeneID oldGeneID index world = 
        let oldSpecies = world |> Map.find specID
        let newGeneID = (specID, newGeneID)
        let oldGeneID = (specID, oldGeneID)
        let oldGene = oldSpecies |> Map.find oldGeneID

        let newOldDNA = oldGene.DNA.Substring(0, index)
        let newDNA = oldGene.DNA.Remove(0, index)

        let rightOrigins = getSplitRight oldGene.origins index 0
        let leftOrigins = getSplitLeft oldGene.origins index 0

        let newGene = {origins = rightOrigins; DNA = newDNA}
        let newOldGene = {origins = leftOrigins; DNA = newOldDNA}

        world.Add(specID, world.[specID].Add(newGeneID, newGene).Add(oldGeneID, newOldGene))
//----------------------------------------------------------------------------------------------//
    let runFusion specID fstGeneID sndGeneID world =
        let species = world |> Map.find specID
        let fstGeneID = (specID, fstGeneID)
        let fstGene = species |> Map.find fstGeneID
        let sndGeneID = (specID, sndGeneID)
        let sndGene = species |> Map.find sndGeneID
        
        let newOrigins = fstGene.origins@sndGene.origins
        let newDNA = String.concat "" [fstGene.DNA;sndGene.DNA] //Merge the DNA of the left and right genes.
        let newGene = {origins = newOrigins; DNA = newDNA}

        world.Add(specID, world.[specID].Add(fstGeneID, newGene).Remove(sndGeneID))
//----------------------------------------------------------------------------------//
    let runSpeciation newSpecID oldSpecID world =
        let oldSpecies = world |> Map.find oldSpecID

        let newID oldGene = ((newSpecID, snd (fst oldGene)), snd oldGene)
        let newSpecies = oldSpecies |> Map.toSeq |> Seq.map (fun x -> newID x) |> Map.ofSeq

        world.Add(newSpecID, newSpecies)
//-----------------------------------------------------------------//
    //'world' is the main map. chooseFunction takes it as a parameter, runs changes on it based on the inputs, and returns the new, changed, world.
    //Takes a string of inputs, and splits it. Checks the event name and calls that function with the necessary parameters.
    let chooseFunction world (inputString:string) =
        let inputs = (inputString.Split(','))
        let specID = (toInt inputs.[1])
        let geneID = (toInt inputs.[2])
        match inputString with
        | Prefix "create" rest -> runCreate (specID) (geneID) inputs.[3] world
        | Prefix "snip" rest-> runSnip (specID) (geneID) (toInt inputs.[3]) inputs.[5].[0] world
        | Prefix "insert" rest -> runInsert specID geneID (toInt inputs.[3]) inputs.[4] world
        | Prefix "delete" rest -> runDelete specID geneID (toInt inputs.[3]) (toInt inputs.[4]) world
        | Prefix "duplicate" rest -> runDuplicate specID geneID (toInt inputs.[3]) world
        | Prefix "loss" rest-> runLoss specID geneID world
        | Prefix "fission" rest -> runFission specID geneID (toInt inputs.[3]) (toInt inputs.[4]) world
        | Prefix "fusion" rest -> runFusion specID geneID (toInt inputs.[3]) world
        | Prefix "speciation" rest -> runSpeciation specID (toInt inputs.[2]) world
        |_ -> world

    //Fold over each line in the input file, accumulating data as a map.
    let evolution = 
        readLines |> Seq.fold chooseFunction (Map.ofList[]) //Map is of type Map<int, Map<(int*int), gene>>
//-------------------------------------------------------------------------------------//
    
    //Take two origins. If they have the same initial ID, check if they overlap.
    let originsMatch originOne originTwo =
        let leftOverlap oriOne oriTwo = (oriTwo.endPos > oriOne.startPos && oriOne.endPos > oriTwo.startPos)
        let rightOverlap oriOne oriTwo = (oriOne.endPos > oriTwo.startPos && oriTwo.endPos > oriOne.startPos) 

        match originOne.species = originTwo.species && originOne.gene = originTwo.gene with
        | true -> 
            match leftOverlap originOne originTwo || rightOverlap originOne originTwo with
            | true -> true
            | false -> false
        | false -> false

    //Take a gene, the world and a list to keep homologies in. Fold over the entire world, adding homologies to the list when origins overlap.
    let homologies world key geneOne homList=
        let iterGenes homList specID species =
            let genesMatch homList geneID geneTwo  = 
                geneOne.origins |> List.fold(fun accA originOne ->  geneTwo.origins |>
                    List.fold (fun accB originTwo ->  match originsMatch originOne originTwo with
                                        | true -> geneID::accB
                                        | false -> accB) 
                                accA) homList
            species |> Map.fold genesMatch homList  
        world |> Map.fold iterGenes homList
    
    //Print a given list of homologies to the .homologs file.
    let printHoms homList =
        homList|> List.sort |> List.distinct |> List.iter(fun x -> fprintf writeHom (" SE%i_G%i") <|| x)
    
    //Iterate over the entire map. Print IDs and DNA to the .fa file when appropriate. Get and print the homologies for each gene.
    evolution |> 
        Map.iter (fun specKey species -> species |> 
            Map.iter (fun geneKey gene -> fprintfn write ">SE%i_G%i" specKey (snd geneKey); fprintf writeHom ("SE%i_G%i:") specKey (snd geneKey); homologies evolution geneKey gene [] |>
                printHoms; gene.DNA |>
                    (fun dna -> fprintf write ("%s") dna); fprintf writeHom ("\n"); fprintf write ("\n")));

    printfn("Complete.")
    0 // return an integer exit code
