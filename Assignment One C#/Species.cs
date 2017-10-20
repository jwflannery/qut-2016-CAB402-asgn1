using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AssignmentOne
{
    public class Species
    {
        public Species(int specID)
        {
            MySpecId = specID;
        }

        //For speciation. Create a new species, with an already existing set of genes.
        public Species(int specID, SortedDictionary<int, Gene> genes)
        {
            MySpecId = specID;
            foreach (var gene in genes)
            {
                Genes.Add(gene.Key, new Gene(MySpecId, gene.Value.gene, gene.Value.Dna, gene.Value.origins));
            }
        }


        public int MySpecId { get; }
        public SortedDictionary<int, Gene> Genes { get; set; } = new SortedDictionary<int, Gene>();


        public void Create(int gene, string dna)
        {
            Genes.Add(gene, new Gene(MySpecId, gene, dna));
        }

        public void Duplicate(int newGeneId, Gene currentGene)
        {

            Gene newGene = new Gene(MySpecId, newGeneId, currentGene.Dna, currentGene.origins);
            Genes.Add(newGeneId, newGene);
        }

        public void Loss(int geneID)
        {
            Genes.Remove(geneID);
        }

        //Split the origins at the index, assign them to the new genes.
        public void Fission(int newGeneID, int oldGeneID, int index, World w)
        {
            string leftDNA = Genes[oldGeneID].Dna.Remove(index);
            string rightDNA = Genes[oldGeneID].Dna.Substring(index);

            List<Origin> leftOrigins = Genes[oldGeneID].splitLeft(index);
            List<Origin> rightOrigins = Genes[oldGeneID].splitRight(index);

            Genes.Remove(oldGeneID);
            Genes.Add(oldGeneID, new Gene(MySpecId, oldGeneID, leftDNA, leftOrigins));

            Genes.Add(newGeneID, new Gene(MySpecId, newGeneID, rightDNA, rightOrigins));
        }

        //Concatinate the DNA and the origins.
        public void Fusion(int fstGeneID, int sndGeneID)
        {
            Genes[fstGeneID].Dna += Genes[sndGeneID].Dna;
            Genes[fstGeneID].origins.AddRange(Genes[sndGeneID].origins);
        }
    }
}
