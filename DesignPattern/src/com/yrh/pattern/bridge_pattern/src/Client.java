package com.yrh.pattern.bridge_pattern.src;

/**
 * 客户端类
 * @author Yrh
 *
 */
public class Client {

	public static void main(String[] args) {
		Abstraction ab = new RefinedAbstraction();
		
		ab.setImplementor(new ConcreteImplementorA());
		ab.operation();
		
		ab.setImplementor(new ConcreteImplementorB());
		ab.operation();
	}
}
