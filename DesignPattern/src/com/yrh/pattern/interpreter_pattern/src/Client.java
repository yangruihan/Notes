package com.yrh.pattern.interpreter_pattern.src;

import java.util.ArrayList;
import java.util.List;

/**
 * 客户端类
 * 
 * @author Yrh
 *
 */
public class Client {

	public static void main(String[] args) {
		Context context = new Context();

		List<AbstractExpression> list = new ArrayList<>();
		list.add(new TerminalExpression());
		list.add(new NonterminalExpression());
		list.add(new TerminalExpression());
		list.add(new TerminalExpression());

		for (AbstractExpression exp : list) {
			exp.interpret(context);
		}
	}
}
