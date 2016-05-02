package com.yrh.pattern.interpreter_pattern.src;

/**
 * 终结符表达式类
 * @author Yrh
 *
 */
public class TerminalExpression extends AbstractExpression {

	@Override
	public void interpret(Context context) {
		System.out.println("终端解释器");
	}
}
