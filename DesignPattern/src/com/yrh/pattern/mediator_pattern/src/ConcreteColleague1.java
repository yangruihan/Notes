package com.yrh.pattern.mediator_pattern.src;

/**
 * 具体同事类1
 * @author Yrh
 *
 */
public class ConcreteColleague1 extends Colleague {

	public ConcreteColleague1(Mediator mediator) {
		super(mediator);
	}

	public void send(String message) {
		mediator.send(message, this);
	}
	
	public void notify(String message) {
		System.out.println("同事1得到消息：" + message);
	}
}
