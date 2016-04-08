package com.yrh.pattern.observer_pattern.src;

/**
 * 抽象观察者接口
 * @author Yrh
 *
 */
public interface Observer {

	/**
	 * 更新函数
	 * @param str 更新状态
	 */
	public void update(String str);
}
