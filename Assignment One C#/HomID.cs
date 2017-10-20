using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AssignmentOne
{
    //Essentially just a species and geneID. Used for easy printing of output.
    public class HomId
    {
        public HomId(int specID, int geneID)
        {
            Species = specID;
            Gene = geneID;
        }
        public int Species { get; }
        public int Gene { get; }
    }
}
