package com.yrh.pattern.bridge_pattern.src;

/**
 * 抽象类
 * @author Yrh
 *
 */
public abstract class Abstraction {

	protected Implementor implementor;

	public void setImplementor(Implementor implementor) {
		this.implementor = implementor;
	}
	
	public abstract void operation();
}
