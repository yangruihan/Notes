package com.yrh.pattern.state_pattern.src;

/**
 * 上下文类
 * @author Yrh
 *
 */
public class Context {

	private State state;
	
	public Context(State state) {
		this.state = state;
	}
	
	public void request() {
		state.handler(this);
	}

	public State getState() {
		return state;
	}

	public void setState(State state) {
		this.state = state;
		System.out.println("当前状态为：" + state.getClass().getName());
	}
}
