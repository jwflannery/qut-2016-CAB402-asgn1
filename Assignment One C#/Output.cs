using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AssignmentOne
{
    public class Output
    {
        public void Write(string line)
        {
            using (var output = new System.IO.StreamWriter(Globals.FileName + ".fa", true))
                output.Write(line);
        }

        public void WriteHom(string line)
        {
            using (var outputHom = new System.IO.StreamWriter(Globals.FileName + ".homologs", true))
                outputHom.Write(line);
        }
    }
}
