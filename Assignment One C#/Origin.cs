namespace AssignmentOne
{
    //Holds the ID and start/end positions of a creation or insert event.
    public class Origin
    {
        public Origin(int specID, int geneID, int start, int end)
        {
            Species = specID;
            Gene = geneID;
            StartPos = start;
            EndPos = end;
        }

        public int Species { get; }
        public int Gene { get; }
        public int StartPos { get; }
        public int EndPos { get; }
    }
}