package com.yrh.pattern.memento_pattern.src;

/**
 * 管理者类
 * @author Yrh
 *
 */
public class Caretaker {
	private Memento memento;

	public Memento getMemento() {
		return memento;
	}

	public void setMemento(Memento memento) {
		this.memento = memento;
	}
}
