package com.yrh.pattern.flyweight_pattern.src;

/**
 * 客户端类
 * 
 * @author Yrh
 *
 */
public class Client {

	public static void main(String[] args) {
		int e = 22;

		FlyweightFactory f = new FlyweightFactory();
		
		Flyweight fx = f.getFlyweight("X");
		fx.operation(--e);
		
		Flyweight fy = f.getFlyweight("Y");
		fy.operation(--e);
		
		Flyweight fz = f.getFlyweight("Z");
		fz.operation(--e);
		
		Flyweight uf = new UnsharedConcreteFlyweight();
		uf.operation(--e);;
	}
}
