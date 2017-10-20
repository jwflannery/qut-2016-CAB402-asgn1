open System.Collections.Generic

//This program reads a series of evolution commands from a file, runs these, and prints the resulting data to .fa and .homolog files, in their respecive formats.
//Data is stored as a 
//SortedDictionary<int, SortedDictionary<(int*int), gene>>
//The outer map holds what is essentially each species, which in turn hold each gene.
//The (int*int) key holds the current ID of the gene, specID being first and GeneID second.

//-----------------------------------------------------------------------------------------------------//

//Holds the species and int that a 'segment' of DNA originated from,
//as well as where this element starts and ends, relative to that original string.
type Origin (species, gene, startPos, endPos)  =
    member this.Species = species
    member this.Gene = gene
    member this.StartPos = startPos
    member this.EndPos = endPos

//Holds the origins of every segment of its DNA, as well as the DNA itself. Also holds its own Species and Gene IDs, as ints.
type Gene (species, gene, origins, dna) = 
    member this.Species = species
    member this.Gene = gene
    member this.Origins = origins
    member this.DNA = dna


[<EntryPoint>]
let main argv = 
    let filename = argv.[0]
    let world = new SortedDictionary<int, SortedDictionary<(int*int), Gene>>(); //The dictionary to store all the data in.
    let readLines =  System.IO.File.ReadLines(filename)

    use write = new System.IO.StreamWriter(filename + ".fa", false)
    use writeHom = new System.IO.StreamWriter(filename + ".homologs", false)

    let toInt X = System.Int32.Parse X

    //Current length of an origin.
    let len (x:Origin) = 
        x.EndPos - x.StartPos + 1

    //Active pattern to match a string against a given prefix.
    //Credit to whoever posted this to StackOverflow ages ago.
    let (|Prefix|_|) (p:string) (s:string) =
        if s.StartsWith(p) then
            Some(s.Substring(p.Length))
        else
            None

    //Recursively matches each element of the origin list. If it ends before the index, discard it. If it starts afterwards, add it to the return list.
    //Index tracks where to split, updating to account for already processed origins.

    //'currentPoint' tracks the start of the currently processing string. It should always be called at 0 initially.
    //Surely there's a better way to handle that, no?
    let rec getSplitRight (originList: Origin list) index currentPoint =
        let splitOrigin (origin:Origin) index = 
            new Origin(origin.Species, origin.Gene, index+origin.StartPos, origin.EndPos) //Take an origin and give a new one that starts at the given index.
        match originList with
        | [] -> []
        | head::tail -> match ((len head) + currentPoint > index) with
            | true -> match (currentPoint < index) with
                | false -> head :: getSplitRight tail (index - len head) (currentPoint + len head)
                | true -> (splitOrigin head index) :: getSplitRight tail (index - len head) (currentPoint + len head)
            | false -> getSplitRight tail (index - len head) (currentPoint) 

    //Same thing as getSplitRight, but this time it takes the origins to the left of the index.
    let rec getSplitLeft originList index currentPoint = 
        let splitOrigin (origin:Origin) index = 
            new Origin (origin.Species, origin.Gene, origin.StartPos, origin.StartPos + index-1)
        match originList with
        | [] -> []
        | head::tail -> match (currentPoint < index) with
            | true -> match ((len head) + currentPoint < index) with
                | true -> head :: getSplitLeft tail (index - len head) (currentPoint) 
                | false -> splitOrigin head index :: getSplitLeft tail (index - len head) (currentPoint + len head) 
            | false -> getSplitLeft tail (index - len head) (currentPoint) 

//--------------------------------------------------------------------------------------//
    //Takes the elements for a gene, creates a gene, adds the gene to the map.
    let runCreate specID geneID newDNA =
        let newGene = new Gene(specID, geneID, [new Origin(specID, geneID, 0, (newDNA |> String.length )-1)], newDNA)

        let newSpec = new SortedDictionary<(int*int) , Gene>();
        newSpec.Add((specID, geneID), newGene)
        
        //If the species already exists, no need to add it.
        match world.ContainsKey specID with 
        |false -> world.Add(specID, newSpec)
        |true -> world.[specID].Add((specID, geneID), newGene)
// //------------------------------------------------------------------//
    //Iterate over the characters in a string. When the index is reached, the character is replaced by newNuc.
    let snipGene index gene newNuc = String.mapi (fun i c -> if i = index then newNuc else c) gene

    //Snips the gene, creates the new one. Removes the old from the dictionary and adds the new.
    let runSnip specID geneID index newNuc =
        let currentSpecies = world.[specID]
        let currentGene = currentSpecies.[specID, geneID]


        let newDNA = (snipGene index currentGene.DNA newNuc)
        let newGene = new Gene(specID, geneID, currentGene.Origins, newDNA)

        currentSpecies.Remove(specID, geneID)
        currentSpecies.Add((specID, geneID), newGene)

////--------------------------------------------------------------------------------------------//
    let runInsert specID geneID index dnaSeg =
        let currentSpecies = world.[specID]
        let currentGene = currentSpecies.[specID, geneID]

        let newDNA = currentGene.DNA.Insert(index, dnaSeg)
        let newOrigin = new Origin(specID, geneID*10, 0, dnaSeg.Length - 1) //Yes, this is totally a hack.

        //Split the origins at the point the new DNA is to be inserted
        let leftOrigins = getSplitLeft currentGene.Origins index 0
        let rightOrigins = getSplitRight currentGene.Origins index 0

        let newGene = new Gene(specID, geneID, leftOrigins@[newOrigin]@rightOrigins, newDNA)

        currentSpecies.Remove(specID, geneID)
        currentSpecies.Add((specID, geneID), newGene)
////---------------------------------------------------------------------------------//
    let runDelete specID geneID index length =
        let currentSpecies = world.[specID]
        let currentGene = currentSpecies.[specID, geneID]

        let leftOrigins = getSplitLeft currentGene.Origins index 0
        let rightOrigins = getSplitRight currentGene.Origins (index + length) 0 //Gets the origins to the right of the DNA being deleted.

        let newDNA = currentGene.DNA.Remove(index, length)
        let newGene = new Gene(specID, geneID, leftOrigins@rightOrigins, newDNA) //Origins to the left and right of the deleted DNA are concatenated.

        currentSpecies.Remove(specID, geneID)
        currentSpecies.Add((specID, geneID), newGene)
// //-------------------------------------------------------------------------------------------//           
    let runDuplicate specID newGeneID oldGeneID =
        let oldSpecies = world.[specID]
        let oldGene = oldSpecies.[specID, oldGeneID]
        let newGene = new Gene(specID, newGeneID, oldGene.Origins, oldGene.DNA)

        oldSpecies.Add((specID, newGeneID), newGene)
////--------------------------------------------------------------------------------//
    let runLoss specID geneID =
        let species = world.[specID]
        let geneID = (specID, geneID)

        species.Remove(geneID)
        () //.Remove returns a bool for some reason, but this function needs to be of type 'unit'.
////-------------------------------------------------------------------------------//
    let runFission specID rightGeneID leftGeneID index = 
        let Species = world.[specID]
        let newGeneID = (specID, rightGeneID)
        let oldGeneID = (specID, leftGeneID)
        let oldGene = Species.[oldGeneID]
        let endPos = oldGene.DNA |> String.length |> sprintf "%i"

        let leftDNA = oldGene.DNA.Substring(0, index)
        let rightDNA = oldGene.DNA.Remove(0, index)
        let rightOrigins = getSplitRight oldGene.Origins index 0
        let leftOrigins = getSplitLeft oldGene.Origins index 0
        let newGene = new Gene(specID, rightGeneID, rightOrigins, rightDNA)
        let newOldGene = new Gene(specID, leftGeneID, leftOrigins, leftDNA)

        Species.Remove(oldGeneID);
        Species.Add(oldGeneID, newOldGene);
        Species.Add(newGeneID, newGene);
////----------------------------------------------------------------------------------------------//
    let runFusion specID fstGeneID sndGeneID =
        let species = world.[specID]
        let leftGeneID = (specID, fstGeneID)
        let leftGene = species.[leftGeneID]
        let rightGeneID = (specID, sndGeneID)
        let rightGene = species.[rightGeneID]
        
        let newOrigins = leftGene.Origins@rightGene.Origins
        let newDNA = String.concat "" [leftGene.DNA;rightGene.DNA] //Merge the DNA of the left and right genes.
        let newGene = new Gene(specID, fstGeneID, newOrigins, newDNA)

        species.Remove(leftGeneID);
        species.Remove(rightGeneID);
        species.Add(leftGeneID, newGene)

////----------------------------------------------------------------------------------//
    let runSpeciation newSpecID oldSpecID =
        let oldSpecies = world.[oldSpecID]

        let newGene (oldGene:Gene) = new Gene(newSpecID, oldGene.Gene, oldGene.Origins, oldGene.DNA)

        let newSpecies = new SortedDictionary<(int*int), Gene>()

        for gene in oldSpecies do
            newSpecies.Add((newSpecID, gene.Value.Gene), newGene gene.Value)

        world.Add(newSpecID, newSpecies)
//-----------------------------------------------------------------//
    //Takes a string of inputs, and splits it. Checks the event name and calls that function with the necessary parameters.
    let chooseFunction (inputString:string) =
        let inputs = (inputString.Split(','))
        let specID = (toInt inputs.[1])
        let geneID = (toInt inputs.[2])
        match inputString with
        | Prefix "create" rest -> runCreate (specID) (geneID) inputs.[3]
        | Prefix "snip" rest-> runSnip (specID) (geneID) (toInt inputs.[3]) inputs.[5].[0]
        | Prefix "insert" rest -> runInsert specID geneID (toInt inputs.[3]) inputs.[4]
        | Prefix "delete" rest -> runDelete specID geneID (toInt inputs.[3]) (toInt inputs.[4])
        | Prefix "duplicate" rest -> runDuplicate specID geneID (toInt inputs.[3])
        | Prefix "loss" rest-> runLoss specID geneID
        | Prefix "fission" rest -> runFission specID geneID (toInt inputs.[3]) (toInt inputs.[4])
        | Prefix "fusion" rest -> runFusion specID geneID (toInt inputs.[3])
        | Prefix "speciation" rest -> runSpeciation specID (toInt inputs.[2])
        |_ -> ()

    //Iterate over each line 
    let evolution = 
        readLines |> Seq.iter chooseFunction


    //Take two origins. If they have the same initial ID, check if they overlap.
    let originsMatch (geneOne:Gene) (geneTwo:Gene) =
        let leftOverlap (oriOne:Origin) (oriTwo:Origin) = (oriTwo.EndPos > oriOne.StartPos && oriOne.EndPos > oriTwo.StartPos)
        let rightOverlap (oriOne:Origin) (oriTwo:Origin) = (oriOne.EndPos > oriTwo.StartPos && oriTwo.EndPos > oriOne.StartPos) 

        match geneOne.Species = geneTwo.Species && geneOne.Gene = geneTwo.Gene with
        | true -> true
        | false -> 
                match geneOne.Origins |> List.exists (fun originOne -> geneTwo.Origins |> List.exists(fun originTwo -> (leftOverlap originOne originTwo || rightOverlap originOne originTwo) && originOne.Species = originTwo.Species && originOne.Gene = originTwo.Gene)) with
                | true -> true
                | false -> false

    //Take a gene to check. Match it with every gene in the world, and add any matches.
    let getHomologies (geneOne:Gene) =
        let mutable homologies = []
        for species in world do
            for gene in species.Value do
                match originsMatch geneOne gene.Value with
                | true -> homologies <- gene.Key::homologies
                | false -> ()
        homologies

    //Take a gene, get the homologies, and print them.
    let printHoms (geneOne:Gene) =
        fprintf writeHom ("SE%i_G%i:") geneOne.Species geneOne.Gene
        let mutable homologies = []

        homologies <- (getHomologies geneOne)
        for hom in homologies |> List.sort do
            fprintf writeHom (" SE%i_G%i") <|| hom

        fprintf writeHom ("\n")

    //Print the dna and homologies of every gene in the world.
    let printAll = 
        for species in world do
            for gene in species.Value do
                fprintf write (">SE%i_G%i\n") gene.Value.Species gene.Value.Gene
                fprintf write ("%s\n") gene.Value.DNA
                printHoms gene.Value

    printfn("Complete.")
    0 // return an integer exit code
