using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AssignmentOne
{
    public class World
    {

        public SortedDictionary<int, Species> Species { get; set; } = new SortedDictionary<int, Species>(); //Dictonary used to hold all the data.
        public World(IEnumerable<string> inputLines)
        {
            //For some reason it doesn't like to just overwrite files, so we remove them first.
            System.IO.File.Delete(Globals.FileName + ".fa");
            System.IO.File.Delete(Globals.FileName + ".homologs");
            
            //Create an empty species to start with.
            Species.Add(1, new Species(1));

            //Iterate over each line in the input, calling the appropriate function and giving the necessary parameters.
            foreach (var line in inputLines)
            {
                string[] parameters = line.Split(',');

                switch (parameters[0])
                {
                    case "create":
                        ParseCreate(parameters);
                        break;
                    case "snip":
                        ParseSnip(parameters);
                        break;
                    case "insert":
                        ParseInsert(parameters);
                        break;
                    case "delete":
                        ParseDelete(parameters);
                        break;
                    case "duplicate":
                        ParseDuplicate(parameters);
                        break;
                    case "loss":
                        ParseLoss(parameters);
                        break;
                    case "fission":
                        ParseFission(parameters);
                        break;
                    case "fusion":
                        ParseFusion(parameters);
                        break;
                    case "speciation":
                        Speciation(parameters);
                        break;
                }
            }
        }

        //If the given species exists, add a new gene. Otherwise, add a new species and then a new gene.
        private void ParseCreate(string[] parameters)
        {
            int inSpecies = Convert.ToInt32(parameters[1]);
            int inGene = Convert.ToInt32(parameters[2]);
            string inDna = parameters[3];

            if (Species.ContainsKey(inSpecies))
            {
                Species[inSpecies].Create(inGene, inDna);
            }
            else
            {
                Species.Add(inSpecies, new Species(inSpecies));
                Species[inSpecies].Create(inGene, inDna);
            }
        }

        private void ParseSnip(string[] parameters)
        {
                int inSpecies = Convert.ToInt32(parameters[1]);
                int inGene = Convert.ToInt32(parameters[2]);
                int index = Convert.ToInt32(parameters[3]);
                string oldNuc = parameters[4];
                string newNuc = parameters[5];

                Species[inSpecies].Genes[inGene].Snip(index, oldNuc, newNuc);
        }

        private void ParseInsert(string[] parameters)
        {
            int inSpecies = Convert.ToInt32(parameters[1]);
            int inGene = Convert.ToInt32(parameters[2]);
            int index = Convert.ToInt32(parameters[3]);
            string newDNA = parameters[4];

            Species[inSpecies].Genes[inGene].Insert(index, newDNA);
        }

        private void ParseDelete(string[] parameters)
        {
            int inSpecies = Convert.ToInt32(parameters[1]);
            int inGene = Convert.ToInt32(parameters[2]);
            int index = Convert.ToInt32(parameters[3]);
            int length = Convert.ToInt32(parameters[4]);

            Species[inSpecies].Genes[inGene].Delete(index, length, this);
        }

        private void ParseDuplicate(string[] parameters)
        {
            int inSpecies = Convert.ToInt32(parameters[1]);
            int newGeneID = Convert.ToInt32(parameters[2]);
            int currentGeneID = Convert.ToInt32(parameters[3]);
            Gene currentGene = Species[inSpecies].Genes[currentGeneID];

            Species[inSpecies].Duplicate(newGeneID, currentGene);
        }

        public void ParseLoss(string[] parameters)
        {
            int inSpecies = Convert.ToInt32(parameters[1]);
            int inGene = Convert.ToInt32(parameters[2]);

            Species[inSpecies].Loss(inGene);
        }
        private void ParseFission(string[] parameters)
        {
            int inSpecies = Convert.ToInt32(parameters[1]);
            int newGeneID = Convert.ToInt32(parameters[2]);
            int oldGeneID = Convert.ToInt32(parameters[3]);
            int index = Convert.ToInt32(parameters[4]);

            Species[inSpecies].Fission(newGeneID, oldGeneID, index, this);

        }

        //Fuse two genes, then remove the dead gene.
        private void ParseFusion(string[] parameters)
        {
            int inSpecies = Convert.ToInt32(parameters[1]);
            int fstGeneId = Convert.ToInt32(parameters[2]);
            int sndGeneID = Convert.ToInt32(parameters[3]);

            Species[inSpecies].Fusion(fstGeneId, sndGeneID);
            ParseLoss(new string[] {"", parameters[1], parameters[3]});

        }
        //Create a new species that matches the old, and add it to the map.
        private void Speciation(string[] parameters)
        {
            int newSpec = Convert.ToInt32(parameters[1]);
            int oldSpec = Convert.ToInt32(parameters[2]);

            Species newSpecies = new Species(newSpec, Species[oldSpec].Genes);
            Species.Add(newSpec, newSpecies);
        }
        //Check if there are any matches between two genes.
        public bool CheckHomologies(Gene geneOne, Gene geneTwo)
        {
            if (geneOne.Species == geneTwo.Species && geneOne.gene == geneTwo.gene)
            {
                return true;
            }
            //If there are any overlaps between any of the origins in the two genes, return true.
            //ReSharper converted it to LINQ for me. Fancy, right?
            return (from oriOne in geneOne.origins from oriTwo in geneTwo.origins where oriOne.Species == oriTwo.Species && oriOne.Gene == oriTwo.Gene where 
                    (oriOne.EndPos >= oriTwo.StartPos && oriTwo.EndPos >= oriOne.StartPos) || oriTwo.EndPos >= oriOne.StartPos && oriOne.EndPos >= oriTwo.StartPos select oriOne).Any();
        }

        public List<HomId> GetHomologies(Gene geneOne)
        {
            //Check a gene for matches with any other genes in the world. ReSharper fancied this one up, too. 
            return (from spec in Species from gene in spec.Value.Genes where CheckHomologies(geneOne, gene.Value) select new HomId(gene.Value.Species, gene.Value.gene)).ToList();
        }

        //Get and print the homologies for a given gene.
        private void PrintHoms(Output output, KeyValuePair<int, Gene> gene)
        {
            output.WriteHom("SE" + gene.Value.Species + "_G" + gene.Value.gene + ":");
            var homologies = GetHomologies(gene.Value);

            foreach (var hom in homologies)
            {
                output.WriteHom(" SE" + hom.Species + "_G" + hom.Gene);
            }
            output.WriteHom("\n");
        }

        //Print the DNA and Homologies for every gene in the world.
        public void PrintAll()
        {
            foreach (var spec in Species)
            {
                foreach (var gene in spec.Value.Genes)
                {
                    Globals.Output.Write(">SE" + gene.Value.Species + "_G" + gene.Value.gene + "\n");
                    Globals.Output.Write(gene.Value.Dna + "\n");
                    PrintHoms(Globals.Output, gene);
                }
            }
        }
    }
}
