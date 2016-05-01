package com.yrh.pattern.memento_pattern.src;

/**
 * 备忘录类
 * @author Yrh
 *
 */
public class Memento {
	private String state; // 状态
	
	public Memento(String state) {
		this.state = state;
	}

	public String getState() {
		return state;
	}
}
