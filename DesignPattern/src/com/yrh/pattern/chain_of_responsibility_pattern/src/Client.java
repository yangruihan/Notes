package com.yrh.pattern.chain_of_responsibility_pattern.src;

/**
 * 客户端类
 * 
 * @author Yrh
 *
 */
public class Client {

	public static void main(String[] args) {
		Handler h1 = new ConcreteHandler1();
		Handler h2 = new ConcreteHandler2();
		Handler h3 = new ConcreteHandler3();
		h1.setSuccessor(h2);
		h2.setSuccessor(h3);

		int[] requests = { 2, 5, 14, 22, 18, 3, 28, 20 };
		
		for (int r : requests) {
			h1.handleRequest(r);
		}
	}
}
