package com.yrh.pattern.abstract_factory_pattern.src;

/**
 * 具体工厂1
 * @author Yrh
 *
 */
public class ConcreteFactory1 extends Factory {

	@Override
	public ProductA getProductA() {
		return new ProductA1();
	}

	@Override
	public ProductB getProductB() {
		return new ProductB1();
	}

}
