namespace Aoc.Extensions
{
    public static class StringExtensions
    {
        public static int BitCommonality(this string expression)
        {
            int commonality = 0;
            foreach (char bit in expression)
            {
                switch (bit)
                {
                    case '0':
                        commonality += 1;
                        break;
                    case '1':
                        commonality -= 1;
                        break;
                }
            }
            return commonality;
        }
    }
}