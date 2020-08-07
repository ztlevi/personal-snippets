    class TrieNode {
        List<String> startWith;
        Map<Character, TrieNode> children;

        TrieNode() {
            startWith = new ArrayList();
            children = new HashMap();
        }
    }

    class Trie {
        TrieNode root;

        Trie(String[] words) {
            root = new TrieNode();
            for (String w : words) {
                TrieNode cur = root;
                for (char ch : w.toCharArray()) {
                    TrieNode tnode = cur.children.getOrDefault(ch, new TrieNode());
                    tnode.startWith.add(w);
                    cur.children.put(ch, tnode);
                    cur = cur.children.get(ch);
                }
            }
        }

        List<String> findByPrefix(String prefix) {
            List<String> ans = new ArrayList();
            TrieNode cur = root;
            for (char ch : prefix.toCharArray()) {
                if (!cur.children.containsKey(ch)) {
                    return ans;
                }
                cur = cur.children.get(ch);
            }
            ans.addAll(cur.startWith);
            return ans;
        }
    }