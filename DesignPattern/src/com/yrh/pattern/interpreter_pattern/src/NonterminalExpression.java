package com.yrh.pattern.interpreter_pattern.src;

/**
 * 非终结符表达式类
 * @author Yrh
 *
 */
public class NonterminalExpression extends AbstractExpression {

	@Override
	public void interpret(Context context) {
		System.out.println("非终端解释器");
	}
}
