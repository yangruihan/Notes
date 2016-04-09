package com.yrh.pattern.prototype_pattern.src;

/**
 * 客户端类
 * @author Yrh
 *
 */
public class Client {
	public static void main(String[] args) {
		ConcretePrototype cp = new ConcretePrototype();
		
		for (int i = 0; i < 10; i++) {
			ConcretePrototype clonecp = (ConcretePrototype)cp.clone();
			clonecp.show();
		}
	}
}
