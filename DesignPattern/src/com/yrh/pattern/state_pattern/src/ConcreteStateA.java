package com.yrh.pattern.state_pattern.src;

/**
 * 具体状态类A
 * @author Yrh
 *
 */
public class ConcreteStateA extends State {

	@Override
	public void handler(Context context) {
		// 设置下一状态
		context.setState(new ConcreteStateB());
	}
}
