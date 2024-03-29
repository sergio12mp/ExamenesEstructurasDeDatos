/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 *
 * AVL trees implementation
 */
 
package dataStructures.searchTree;

import java.util.Iterator;
import java.util.NoSuchElementException;
import dataStructures.either.Either;
import dataStructures.either.Left;
import dataStructures.either.Right;
import dataStructures.stack.LinkedStack;
import dataStructures.stack.Stack;
import dataStructures.tuple.Tuple2;

/**
 * Search tree implemented using a balanced AVL tree. Note that keys should define an
 * order relation ({@link java.lang.Comparable}).
 * @param <K> Type of keys.
 * @param <V> Type of values.
 */
public class AVL<K extends Comparable<? super K>, V> implements SearchTree<K, V> {
	private static class Tree<K,V> {
		K key;
		V value;
		int height;
		Tree<K,V> left;
		Tree<K,V> right;

		public Tree(K k, V v) {
			key = k;
			value = v;
			height = 1;
			left = null;
			right = null;
		}
		
		public static int height(Tree<?,?> tree) {
			return tree==null ? 0 : tree.height;
		}
		
		public boolean rightLeaning() {
			return height(right) > height(left);
		}

		public boolean leftLeaning() {
			return height(right) < height(left);
		}

		void setHeight() {
			height = 1 + Math.max(height(left), height(right));
		}
		
		public Tree<K,V> rotR() {
			Tree<K,V> lt = this.left;
			Tree<K,V> lrt = lt.right;
			
			this.left = lrt;
			this.setHeight();
			
			lt.right = this;
			lt.setHeight();
			
			return lt;
		}
		
		public Tree<K,V> rotL() {
			Tree<K,V> rt = this.right;
			Tree<K,V> rlt = rt.left;
			
			this.right = rlt;
			this.setHeight();
			
			rt.left = this;
			rt.setHeight();
			
			return rt;
		}
		
		// balance receiving node. Returns node already balanced
		public Tree<K,V> balance() {
			int lh = height(left);
			int rh = height(right);
			
			Tree<K,V> balanced;
			
			if(lh - rh > 1  && left.rightLeaning()) {
				left = left.rotL();
				balanced = this.rotR();
			} else if (lh - rh > 1) { 
				balanced = this.rotR();
			} else if(rh - lh > 1  && right.leftLeaning()) {
				right = right.rotR();
				balanced = this.rotL();
			} else if (rh - lh > 1) {
				balanced = this.rotL();
			} else {
				balanced = this; //no rotation needed
				balanced.setHeight(); 
			}	
			return balanced;
				
		}

		interface Predicate<T> {
			boolean apply(T x);
		}
		
		public static <K> boolean all(Predicate<K> p, Tree<K,?> tree) {
			if(tree==null)
				return true;
			else
				return (p.apply(tree.key) && all(p,tree.left) && all(p,tree.right));
		}	
	
		public static <K extends Comparable<? super K>> boolean isAVL(final Tree<K,?> tree) {
			if(tree==null)
				return true;
			else {
				Predicate<K> lesser = new Predicate<K>() { 
    	   			public boolean apply(K k){return k.compareTo(tree.key) < 0;}
    	   		};
    	   		
				Predicate<K> greater = new Predicate<K>() { 
    	   			public boolean apply(K k){return k.compareTo(tree.key) > 0;}
    	   		};
    	   		
				return (Math.abs(height(tree.left)-height(tree.right)) < 2)
				        && all(lesser,tree.left) //less(tree.key,tree.left)
				        && all(greater,tree.right) //greater(tree.key,tree.right)
				        && isAVL(tree.left)
				        && isAVL(tree.right);
			}
		}
	}

	private Tree<K,V> root;
	private int size;

	/**
	 * Creates an empty AVL tree.
	 * <p>Time complexity: O(1)
	 */
	public AVL() {
		root = null;
		size = 0;
	}

	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: O(1)
	 */
	public boolean isEmpty() {
		return root == null;
	}
	
	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: O(1)
	 */
	public int size() {
		return size;
	}
	
	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: O(log n)
	 */
	public int height() {
		return Tree.height(root);
	}
	
	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: O(log n)
	 */
	public V search(K key) {
		return searchRec(root, key);
	}

	private static <K extends Comparable<? super K>,V>
	        V searchRec(Tree<K,V> tree, K key) {
		if (tree == null)
			return null; 
		else if (key.compareTo(tree.key) == 0)
			return tree.value;
		else if (key.compareTo(tree.key) < 0)
			return searchRec(tree.left, key);
		else
			return searchRec(tree.right, key);
	}

	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: O(log n)
	 */
	public boolean isElem(K key) {
		return search(key) != null;
	}	
		
	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: O(log n)
	 */
	public void insert(K k, V v) {
		root = insertRec(root, k, v);
	}

	// returns modified tree
	private Tree<K,V> insertRec(Tree<K,V> node, K key, V value) {
		if (node == null) {
			node = new Tree<K,V>(key, value);
			size++;
		} else if (key.compareTo(node.key) == 0)
			node.value = value;
		else if (key.compareTo(node.key) < 0) {
			node.left = insertRec(node.left, key, value);
			node = node.balance();
		} else {
			node.right = insertRec(node.right, key, value);
			node = node.balance();
		}	
		return node;
	}
	
	
	
	
	// pre: node is a non-empty tree
	// Removes minimum key (and value) from tree rooted at node. Before
	// deletion, key and value are saved into temp node.
	// returns modified tree (without min key and value)
	private static  <K extends Comparable<? super K>,V>
	        Tree<K,V> split(Tree<K,V> node, Tree<K,V> temp) {
		if (node.left == null) {
			// min node found, so copy min key and value
			temp.key = node.key;
			temp.value = node.value;
			return node.right; // remove node
		} else {
			// remove min from left subtree
			node.left = split(node.left, temp);
			node = node.balance();
			return node;
		}
	}

	/** 
	 * {@inheritDoc}
	 * <p>Time complexity: O(log n)
	 */
	public void delete(K key) {
		root = deleteRec(root, key);
		// assert Tree.isAVL(root) : "AVL.delete: !isAVL";
	}

	// returns modified tree
	private Tree<K,V> deleteRec(Tree<K,V> node, K key) {
		if (node == null)
			; // key not found; do nothing
		else if (key.compareTo(node.key) == 0) {
			if (node.left == null)
				node = node.right;
			else if (node.right == null)
				node = node.left;
			else {
				node.right = split(node.right, node);
			  node = node.balance();
			}
			size--;
		} else if (key.compareTo(node.key) < 0) {
			node.left = deleteRec(node.left, key);
			node = node.balance();
		} else {
			node.right = deleteRec(node.right, key);
			node = node.balance();
		}	
		return node;
	}	
	
	
	
	// Almost an iterator on nodes in tree
	private abstract class Traversal {
		Stack<Either<Tree<K,V>, Tree<K,V>>> stack = new LinkedStack<Either<Tree<K,V>, Tree<K,V>>>();

		abstract void save(Tree<K,V> node);

		public Traversal() {
			if (root != null)
				save(root);
		}

		public boolean hasNext() {
			return !stack.isEmpty();
		}

		public Tree<K,V> nextTree() {
			if (!hasNext())
				throw new NoSuchElementException();

			Either<Tree<K,V>, Tree<K,V>> either = stack.top();
			stack.pop();

			while (either.isRight()) {
				Tree<K,V> node = either.right();
				save(node);
				either = stack.top();
				stack.pop();
			}
			return either.left();
		}

		public void remove() {
			throw new UnsupportedOperationException();
		}

	}

	private class InOrderTraversal extends Traversal {
		void save(Tree<K,V> node) {
			// in reverse order, cause stack is LIFO
			if (node.right != null)
				stack.push(new Right<Tree<K,V>, Tree<K,V>>(node.right));
			stack.push(new Left<Tree<K,V>, Tree<K,V>>(node));
			if (node.left != null)
				stack.push(new Right<Tree<K,V>, Tree<K,V>>(node.left));
		}
	}
	
	private class PreOrderTraversal extends Traversal {
		void save(Tree<K,V> node) {
			// in reverse order, cause stack is LIFO
			if (node.right != null)
				stack.push(new Right<Tree<K,V>, Tree<K,V>>(node.right));
			if (node.left != null)
				stack.push(new Right<Tree<K,V>, Tree<K,V>>(node.left));
			stack.push(new Left<Tree<K,V>, Tree<K,V>>(node));
		}
	}
	
	private class PostOrderTraversal extends Traversal {
		void save(Tree<K,V> node) {
			// in reverse order, cause stack is LIFO
			stack.push(new Left<Tree<K,V>, Tree<K,V>>(node));
			if (node.right != null)
				stack.push(new Right<Tree<K,V>, Tree<K,V>>(node.right));
			if (node.left != null)
				stack.push(new Right<Tree<K,V>, Tree<K,V>>(node.left));
		}
	}
	
	private class InOrderIt extends InOrderTraversal implements Iterator<K> {
		public K next() {
			return super.nextTree().key;
		}
	}

	private class PreOrderIt extends PreOrderTraversal implements Iterator<K> {
		public K next() {
			return super.nextTree().key;
		}
	}

	private class PostOrderIt extends PostOrderTraversal implements Iterator<K> {
		public K next() {
			return super.nextTree().key;
		}
	}
	
	public Iterable<K> inOrder() {
		return new Iterable<K>(){
			public Iterator<K> iterator() {
				return new InOrderIt();
			}
		};
	}
	
	public Iterable<K> preOrder() {
		return new Iterable<K>(){
			public Iterator<K> iterator() {
				return new PreOrderIt();
			}
		};
	}

	public Iterable<K> postOrder() {
		return new Iterable<K>(){
			public Iterator<K> iterator() {
				return new PostOrderIt();
			}
		};
	}	
	
	
	private class ValuesIt extends InOrderTraversal implements Iterator<V> {
		public V next() {
			return super.nextTree().value;
		}
	}
	
	public Iterable<V> values() {
		return new Iterable<V>(){
			public Iterator<V> iterator() {
				return new ValuesIt();
			}
		};
	}
	
	private class KeysValuesIt extends InOrderTraversal implements Iterator<Tuple2<K,V>> {
		public Tuple2<K,V> next() {
			Tree<K,V> node = super.nextTree();
			return new Tuple2<K,V>(node.key, node.value);
		}
	}
	
	public Iterable<Tuple2<K,V>> keysValues() {
		return new Iterable<Tuple2<K,V>>(){
			public Iterator<Tuple2<K,V>> iterator() {
				return new KeysValuesIt();
			}
		};
	}	

	private static String toStringRec(Tree<?, ?> tree) {
		return tree == null ? "null" : "Node<" + toStringRec(tree.left) + ","
				+ tree.key + "," + tree.value + "," + toStringRec(tree.right) + ">";
	}
	
	/** 
	 * Returns representation of AVL tree as a String.
	 */
  @Override public String toString() {
    String className = getClass().getName().substring(getClass().getPackage().getName().length()+1);  
  	return className+"("+toStringRec(this.root)+")";
  }	
	
}