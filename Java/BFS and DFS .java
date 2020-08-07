//A simple BFS and DFS recursion in Java:
//Just push/offer the root node of the tree in the stack/queue and call these functions.
 
public static void breadthFirstSearch(Queue queue) {
 
    if (queue.isEmpty())
        return;
 
    Node node = (Node) queue.poll();
 
    System.out.println(node + " ");
 
    if (node.right != null)
        queue.offer(node.right);
 
    if (node.left != null)
        queue.offer(node.left);
 
    breadthFirstSearch(queue);
}
 
public static void depthFirstSearch(Stack stack) {
 
    if (stack.isEmpty())
        return;
 
    Node node = (Node) stack.pop();
 
    System.out.println(node + " ");
 
    if (node.right != null)
        stack.push(node.right);
 
    if (node.left != null)
        stack.push(node.left);
 
    depthFirstSearch(stack);
}
