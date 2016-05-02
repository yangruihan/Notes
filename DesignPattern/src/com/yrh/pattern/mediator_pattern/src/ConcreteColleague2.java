package com.yrh.pattern.mediator_pattern.src;

/**
 * 具体同事类2
 * @author Yrh
 *
 */
public class ConcreteColleague2 extends Colleague {

	public ConcreteColleague2(Mediator mediator) {
		super(mediator);
	}
	
	public void send(String message) {
		mediator.send(message, this);
	}

	public void notify(String message) {
		System.out.println("同事2得到信息：" + message);
	}
}
