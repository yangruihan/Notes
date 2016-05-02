package com.yrh.pattern.visitor_pattern.src;

/**
 * 元素抽象类
 * @author Yrh
 *
 */
public abstract class Element {
	public abstract void accept(Visitor visitor);
}
