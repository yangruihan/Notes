package com.yrh.pattern.flyweight_pattern.src;

import java.util.Hashtable;

/**
 * 享元工厂类
 * @author Yrh
 *
 */
public class FlyweightFactory {
	private Hashtable<String, Flyweight> flyweights = new Hashtable<>();
	
	public FlyweightFactory() {
		flyweights.put("X", new ConcreteFlyweight());
		flyweights.put("Y", new ConcreteFlyweight());
		flyweights.put("Z", new ConcreteFlyweight());
	}
	
	public Flyweight getFlyweight(String key) {
		return flyweights.get(key);
	}
}
