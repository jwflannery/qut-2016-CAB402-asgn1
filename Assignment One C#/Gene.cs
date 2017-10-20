using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace AssignmentOne
{
    public class Gene
    {

        public Gene(int species, int gene, string dna)
        {
            origins.Add(new Origin(species, gene, 0, dna.Length-1));
            Dna = dna;
            this.Species = species;
            this.gene = gene;
        }


        //For duplication. Create a gene with an existing set of DNA.
        public Gene(int species, int gene, string dna, List<Origin> origins)
        {
            this.origins.AddRange(origins);
            Dna = dna;
            this.Species = species;
            this.gene = gene;
        }


        public List<Origin> origins = new List<Origin>();
        public string Dna { get; set; }
        public int Species { get; }
        public int gene { get; set; }

        //Get the origins to the right of a given index. 
        public List<Origin> splitRight(int index)
        {
            List<Origin> rightOrigins = new List<Origin>();
            int point = 0;
            foreach (var ori in origins)
            {
                if (point < index)
                {
                    if (point + (ori.EndPos - ori.StartPos) >= index)
                    {
                        Origin split = new Origin(ori.Species, ori.Gene, ori.StartPos + index - point, ori.EndPos);
                        rightOrigins.Add(split);
                        point = index;
                    }
                    else
                    {
                        point += ori.EndPos - ori.StartPos + 1;
                    }
                }
                else
                {
                    point += ori.EndPos - ori.StartPos + 1;
                    rightOrigins.Add(ori);
                }
            }
            return rightOrigins;
        }

        //Origins to the left of the index.
        public List<Origin> splitLeft(int index)
        {
            List<Origin> leftOrigins = new List<Origin>();
            int point = 0;
            foreach (var ori in origins)
            {
                if (point < index)
                {
                    if (point + (ori.EndPos - ori.StartPos) < index)
                    {
                        leftOrigins.Add(ori);
                        point += ori.EndPos - ori.StartPos+1;
                    }
                    else
                    {
                        Origin split = new Origin(ori.Species, ori.Gene, ori.StartPos, ori.StartPos + index - point - 1);
                        leftOrigins.Add(split);
                    }
                }
            }
            return leftOrigins;
        }

        public void Snip(int index, string oldNuc, string newNuc)
        {
                Dna = Dna.Remove(index, 1).Insert(index, newNuc);
                return;
        }

        public void Insert(int index, string newDNA)
        {
            Dna = Dna.Insert(index, newDNA);

            Origin newOrigin = new Origin(Species, gene*10, 0, newDNA.Length-1);

            List<Origin> tempOrigins = splitLeft(index);
            tempOrigins.Add(newOrigin);
            tempOrigins.AddRange(splitRight(index));

            origins = tempOrigins;
        }

        public void Delete(int index, int length, World w)
        {
            Dna = Dna.Remove(index, length);
            List<Origin> tempOrigins = splitLeft(index);
            tempOrigins.AddRange(splitRight(index+length));
            origins = tempOrigins;
        }
    }
}
