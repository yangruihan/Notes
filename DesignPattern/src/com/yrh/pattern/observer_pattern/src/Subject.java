package com.yrh.pattern.observer_pattern.src;

/**
 * 抽象主题接口
 * @author Yrh
 *
 */
public interface Subject {

	/**
	 * 添加观察者
	 * @param observer 观察者
	 */
	public void addObserver(Observer observer);
	
	/**
	 * 删除观察者
	 * @param observer 观察者
	 */
	public void removeObserver(Observer observer);

	/**
	 * 发起通知
	 * @param str 通知状态
	 */
	public void notifyAll(String str);
}
