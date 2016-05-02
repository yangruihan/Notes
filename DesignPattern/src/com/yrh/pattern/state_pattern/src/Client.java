package com.yrh.pattern.state_pattern.src;

/**
 * 客户端类
 * @author Yrh
 *
 */
public class Client {
	
	public static void main(String[] args) {
		Context c = new Context(new ConcreteStateA());
		
		c.request();
		c.request();
		c.request();
		c.request();
	}
}
