package com.yrh.pattern.flyweight_pattern.src;

/**
 * 具体享元类
 * @author Yrh
 *
 */
public class ConcreteFlyweight extends Flyweight {

	@Override
	public void operation(int extrinsicstate) {
		System.out.println("具体 Flyweight：" + extrinsicstate);
	}
}
