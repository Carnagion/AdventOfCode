namespace Aoc.Extensions
{
    public static class CharExtensions
    {
        public static bool IsSameSideBracketAs(this char character, char compare)
        {
            return (character is '(' or '[' or '{' or '<' && compare is '(' or '[' or '{' or '<') || (character is ')' or ']' or '}' or '>' && compare is ')' or ']' or '}' or '>');
        }

        public static int ValidityScore(this char character)
        {
            return character switch
            {
                '(' or ')' => 3,
                '[' or ']' => 57,
                '{' or '}' => 1197,
                '<' or '>' => 25137,
                _ => 0,
            };
        }

        public static char OppositeChar(this char character)
        {
            return character switch
            {
                '(' => ')',
                ')' => '(',
                '[' => ']',
                ']' => '[',
                '{' => '}',
                '}' => '{',
                '<' => '>',
                '>' => '<',
                _ => default,
            };
        }

        public static long EndingScore(this char character)
        {
            return character switch
            {
                ')' => 1,
                ']' => 2,
                '}' => 3,
                '>' => 4,
                _ => 0,
            };
        }

        public static bool IsDigit(this char character)
        {
            return (character - '0').IsBetweenInclusive(0, 9);
        }
    }
}