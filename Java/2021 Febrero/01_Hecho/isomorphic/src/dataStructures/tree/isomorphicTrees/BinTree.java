package dataStructures.tree.isomorphicTrees;

@SuppressWarnings("DuplicatedCode")
public class BinTree<T> {

    private static class Tree<E> {
        private E elem;
        private Tree<E> left;
        private Tree<E> right;

        public Tree(E e, Tree<E> l, Tree<E> r) {
            elem = e;
            left = l;
            right = r;
        }
    }

    private Tree<T> root;

    public BinTree() {
        root = null;
    }

    public BinTree(T x) {
        root = new Tree<>(x, null, null);
    }

    public BinTree(T x, BinTree<T> l, BinTree<T> r) {
        root = new Tree<>(x, l.root, r.root);
    }

    // DO NOT MODIFY CODE ABOVE
    // Please, fill in your data
    //
    // Surname, Name:
    // Group:

    public <E> boolean isomorphic(BinTree<E> that) {

        return isomorphicRec(that.root, this.root);
    }

    private static <A, B> boolean isomorphicRec(Tree<A> t1, Tree<B> t2) {

        boolean [] var = new boolean [1];

        if(t1 == null && t2 == null)
        {
            return true;
        }

        if( (t1 == null && t2 != null) || (t1 != null && t2 == null))
        {
            return false;
        }

        var[0] = true;

        busqueda(var, t1, t2);

        return var[0];

    }

    private static <A, B> void busqueda(boolean [] b, Tree<A> t1, Tree<B> t2) {

        if(t1 == null || t2 == null)
        {

        } else
        {
            if( (t1.right == null && t2.right != null) || (t1.right != null && t2.right == null)
                    || (t1.left == null && t2.left != null) || (t1.left != null && t2.left == null))
            {
                b[0] = false;
            }

            if(t1.left != null && t2.left != null)
            {
                busqueda(b, t1.left, t2.left);
            }


            if(t1.right != null && t2.right != null)
            {
                busqueda(b, t1.right, t2.right);
            }

        }




    }

    // DO NOT MODIFY CODE BELOW

    private static boolean isLeaf(Tree<?> current) {
        return current != null && current.left == null && current.right == null;
    }

    /**
     * Returns representation of tree as a String.
     */
    @Override
    public String toString() {
        return getClass().getSimpleName() + "(" + toStringRec(this.root) + ")";
    }

    private static String toStringRec(Tree<?> tree) {
        return tree == null ? "null" : "Node<" + toStringRec(tree.left) + ","
                + tree.elem + "," + toStringRec(tree.right)
                + ">";
    }

    /**
     * Returns a String with the representation of tree in DOT (graphviz).
     */
    public String toDot(String treeName) {
        final StringBuffer sb = new StringBuffer();
        sb.append(String.format("digraph \"%s\" {\n", treeName));
        sb.append("node [fontname=\"Arial\", fontcolor=red, shape=circle, style=filled, color=\"#66B268\", fillcolor=\"#AFF4AF\" ];\n");
        sb.append("edge [color = \"#0070BF\"];\n");
        toDotRec(root, sb);
        sb.append("}");
        return sb.toString();
    }

    private static void toDotRec(Tree<?> current, StringBuffer sb) {
        if (current != null) {
            final int currentId = System.identityHashCode(current);
            sb.append(String.format("%d [label=\"%s\"];\n", currentId, current.elem));
            if (!isLeaf(current)) {
                processChild(current.left, sb, currentId);
                processChild(current.right, sb, currentId);
            }
        }
    }

    private static void processChild(Tree<?> child, StringBuffer sb, int parentId) {
        if (child == null) {
            sb.append(String.format("l%d [style=invis];\n", parentId));
            sb.append(String.format("%d -> l%d;\n", parentId, parentId));
        } else {
            sb.append(String.format("%d -> %d;\n", parentId, System.identityHashCode(child)));
            toDotRec(child, sb);
        }
    }
}
