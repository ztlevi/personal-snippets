    static public List<Integer> common_divisor(int n) {
        List<Integer> result = new ArrayList();
        for (int i=2; i<=Math.sqrt(n); i++)
        {
            // if 'i' is factor of n
            if (n%i==0)
            {
                // check if divisors are equal
                if (n/i == i)
                    result.add(i);
                else
                    result.add(n/i);
                    result.add(i);
            }
        }
        result.add(n);
        return result;
    }