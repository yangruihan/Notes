package com.yrh.pattern.flyweight_pattern.src;

/**
 * 不需要共享的具体享元类
 * @author Yrh
 *
 */
public class UnsharedConcreteFlyweight extends Flyweight {

	@Override
	public void operation(int extrinsicstate) {
		System.out.println("不共享的具体 Flyweight：" + extrinsicstate);
	}
}
