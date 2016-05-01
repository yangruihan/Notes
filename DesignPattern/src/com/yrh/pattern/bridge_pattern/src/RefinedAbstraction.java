package com.yrh.pattern.bridge_pattern.src;

/**
 * 被提炼的抽象类
 * @author Yrh
 *
 */
public class RefinedAbstraction extends Abstraction {

	@Override
	public void operation() {
		this.implementor.operation();
	}
}
