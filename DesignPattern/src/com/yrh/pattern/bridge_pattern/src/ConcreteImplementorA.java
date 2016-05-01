package com.yrh.pattern.bridge_pattern.src;

/**
 * 具体实现类A
 * @author Yrh
 *
 */
public class ConcreteImplementorA extends Implementor {
	
	@Override
	public void operation() {
		System.out.println("具体实现类A执行的操作");
	}
}
