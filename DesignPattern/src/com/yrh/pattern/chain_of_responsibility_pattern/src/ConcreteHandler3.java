package com.yrh.pattern.chain_of_responsibility_pattern.src;

public class ConcreteHandler3 extends Handler {

	@Override
	public void handleRequest(int request) {
		if (request >= 20 && request < 30) {
			System.out.println(this.getClass().getName() + " 处理请求 " + request);
		} else if (successor != null) {
			successor.handleRequest(request); // 如果继任者不为空，则交由继任者处理
		}
	}
}
