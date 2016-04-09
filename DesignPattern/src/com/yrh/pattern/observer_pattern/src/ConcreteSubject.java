package com.yrh.pattern.observer_pattern.src;

import java.util.ArrayList;
import java.util.List;

/**
 * 具体主题类
 * @author Yrh
 *
 */
public class ConcreteSubject implements Subject {

	// 存放观察者的数组
	private List<Observer> observers = new ArrayList<>();
	
	/**
	 * 添加观察者
	 */
	@Override
	public void addObserver(Observer observer) {
		observers.add(observer);
	}

	/**
	 * 删除观察者
	 */
	@Override
	public void removeObserver(Observer observer) {
		observers.remove(observer);
	}

	/**
	 * 通知所有观察者
	 */
	@Override
	public void notifyAll(String str) {
		for (Observer observer : observers) {
			observer.update(str);
		}
	}

}
