package com.yrh.pattern.abstract_factory_pattern.src;

/**
 * 客户端类
 * @author Yrh
 *
 */
public class Client {

	public static void main(String[] args) {
		Factory factory1 = new ConcreteFactory1();
		ProductA a = factory1.getProductA();
		ProductB b = factory1.getProductB();
		
		a.method();
		b.method();
		
		factory1 = new ConcreteFactory2();
		a = factory1.getProductA();
		b = factory1.getProductB();
		
		a.method();
		b.method();
	}
}
