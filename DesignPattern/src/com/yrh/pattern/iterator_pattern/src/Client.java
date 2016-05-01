package com.yrh.pattern.iterator_pattern.src;

/**
 * 客户端类
 * @author Yrh
 *
 */
public class Client {

	public static void main(String[] args) {
		ConcreteAggregate a = new ConcreteAggregate();
		
		a.add(0, "A");
		a.add(1, "B");
		a.add(2, "C");
		a.add(3, "D");
		a.add(4, "E");
		a.add(5, "F");
		a.add(6, "G");
		
		Iterator iter = a.createIterator();
		
		while (!iter.isDone()) {
			System.out.println(iter.currentItem());
			iter.next();
		}
	}
}
