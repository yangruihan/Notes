package com.yrh.pattern.mediator_pattern.src;

/**
 * 客户端类
 * @author Yrh
 *
 */
public class Client {

	public static void main(String[] args) {
		ConcreteMediator m = new ConcreteMediator();
		
		ConcreteColleague1 c1 = new ConcreteColleague1(m);
		ConcreteColleague2 c2 = new ConcreteColleague2(m);
		
		m.setColleague1(c1);
		m.setColleague2(c2);
		
		c1.send("吃饭了吗？");
		c2.send("没有");
	}
}
