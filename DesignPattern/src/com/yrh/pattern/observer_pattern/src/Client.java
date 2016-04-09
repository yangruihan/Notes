package com.yrh.pattern.observer_pattern.src;

/**
 * 客户端类
 * @author Yrh
 *
 */
public class Client {

	public static void main(String[] args) {
		Subject girl = new ConcreteSubject();
		
		Observer boy1 = new ConcreteObserver();
		Observer boy2 = new ConcreteObserver();
		Observer boy3 = new ConcreteObserver();
		
		girl.addObserver(boy1);
		girl.addObserver(boy2);
		girl.addObserver(boy3);
		
		girl.notifyAll("开心");
	}
}
