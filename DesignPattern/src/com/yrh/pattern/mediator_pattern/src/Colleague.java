package com.yrh.pattern.mediator_pattern.src;

/**
 * 抽象同事类
 * @author Yrh
 *
 */
public abstract class Colleague {
	protected Mediator mediator;
	
	public Colleague(Mediator mediator) {
		this.mediator = mediator;
	}
}
