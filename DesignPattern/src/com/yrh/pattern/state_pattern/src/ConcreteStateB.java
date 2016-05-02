package com.yrh.pattern.state_pattern.src;

/**
 * 具体状态类B
 * @author Yrh
 *
 */
public class ConcreteStateB extends State {

	@Override
	public void handler(Context context) {
		// 设置下一状态
		context.setState(new ConcreteStateA());
	}
}
