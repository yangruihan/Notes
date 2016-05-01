package com.yrh.pattern.bridge_pattern.src;

/**
 * 具体实现类B
 * @author Yrh
 *
 */
public class ConcreteImplementorB extends Implementor {

	@Override
	public void operation() {
		System.out.println("具体实现类B执行的操作");
	}
}
