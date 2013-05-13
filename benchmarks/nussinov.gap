signature Nuss(alphabet, answer) {

  answer nil(void);
  answer right(answer, alphabet);
  answer pair(alphabet, answer, alphabet);
  answer split(answer, answer);
  choice [answer] h([answer]);

}

algebra bpmax implements Nuss(alphabet = char, answer = int)
{
  int nil(void)
  {
    return 0;
  }

  int left(char c, int a)
  {
    return a;
  }

  int pair(char c, int m, char d)
  {
    return m + 1;
  }

  int split(int l, int r)
  {
    return l + r;
  }

  choice [int] h([int] l)
  {
    return list(maximum(l));
  }
  
}

grammar nussinov uses Nuss (axiom=start) {

  tabulated { start, bp }

  start = nil(EMPTY) |
          left(CHAR, start) |
          split(bp, start) # h ;

  bp = pair(CHAR, start, CHAR) with char_basepairing ;

}

instance bpmax = nussinov ( bpmax ) ;