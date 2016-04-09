package com.yrh.pattern.prototype_pattern.src;

/**
 * 原型类
 * @author Yrh
 *
 */
public class Prototype implements Cloneable {
	public Prototype clone() {
		Prototype prototype = null;
		try {
			prototype = (Prototype)super.clone();
		} catch(CloneNotSupportedException e) {
			e.printStackTrace();
		}
		return prototype;
	}
}
