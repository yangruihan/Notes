package com.yrh.pattern.memento_pattern.src;

/**
 * 发起人类
 * @author Yrh
 *
 */
public class Originator {
	private String state;
	
	public Memento createMemento() {
		return new Memento(state);
	}
	
	public void setMemento(Memento memento) {
		state = memento.getState();
	}
	
	public void show() {
		System.out.println("State = " + state);
	}

	public String getState() {
		return state;
	}

	public void setState(String state) {
		this.state = state;
	}
}
