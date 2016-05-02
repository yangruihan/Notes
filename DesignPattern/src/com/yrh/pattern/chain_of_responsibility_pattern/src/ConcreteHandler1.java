package com.yrh.pattern.chain_of_responsibility_pattern.src;

/**
 * 具体处理类1
 * @author Yrh
 *
 */
public class ConcreteHandler1 extends Handler {

	@Override
	public void handleRequest(int request) {
		if (request >= 0 && request < 10) {
			System.out.println(this.getClass().getName() + " 处理请求 " + request);
		} else if (successor != null) {
			successor.handleRequest(request); // 如果继任者不为空，则交由继任者处理
		}
	}
}
