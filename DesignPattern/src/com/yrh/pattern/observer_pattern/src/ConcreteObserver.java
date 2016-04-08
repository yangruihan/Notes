package com.yrh.pattern.observer_pattern.src;

/**
 * 具体观察者类
 * @author Yrh
 *
 */
public class ConcreteObserver implements Observer {

	/**
	 * 当主题发布更新时，执行的操作
	 */
	@Override
	public void update(String str) {
		System.out.println("ConcreteObserver : update : " + str);
	}

}
