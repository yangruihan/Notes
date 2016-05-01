package com.yrh.pattern.iterator_pattern.src;

/**
 * 迭代抽象类
 * @author Yrh
 *
 */
public abstract class Iterator {
	public abstract Object first();
	public abstract Object next();
	public abstract boolean isDone();
	public abstract Object currentItem();
}
