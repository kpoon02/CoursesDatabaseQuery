import {Document, Element, TextNode} from "parse5/dist/tree-adapters/default";
import {InsightError} from "../IInsightFacade";

export class IndexParser {
	public parseDocument(document: Document): Map<string, string> {
		for (let childNode of document.childNodes) {
			if (childNode.nodeName === "html") {
				return this.parseHtml(childNode);
			}
		}
		throw new InsightError("Cannot find HTML");
	}

	private parseHtml(htmlNode: Element): Map<string, string> {
		for (let childNode of htmlNode.childNodes) {
			if (childNode.nodeName === "body") {
				return this.parseBody(childNode);
			}
		}
		throw new InsightError("Cannot find body");
	}

	private parseBody(bodyNode: Element): Map<string, string> {
		for (let childNode of bodyNode.childNodes) {
			if (childNode.nodeName === "div") {
				for (let attr of childNode.attrs) {
					if (attr.name === "class" && attr.value === "full-width-container") {
						return this.parseFwcDivNode(childNode);
					}
				}
			}
		}
		throw new InsightError("Cannot find full-width-container div");
	}

	private parseFwcDivNode(fwcDivNode: Element): Map<string, string> {
		for (let childNode of fwcDivNode.childNodes) {
			if (childNode.nodeName === "div") {
				for (let attr of childNode.attrs) {
					if (attr.name === "id" && attr.value === "main") {
						return this.parseMainDivNode(childNode);
					}
				}
			}
		}
		throw new InsightError("Cannot find main div");
	}

	private parseMainDivNode(mainDivNode: Element): Map<string, string> {
		for (let childNode of mainDivNode.childNodes) {
			if (childNode.nodeName === "div") {
				for (let attr of childNode.attrs) {
					if (attr.name === "id" && attr.value === "content") {
						return this.parseContentDivNode(childNode);
					}
				}
			}
		}
		throw new InsightError("Cannot find content div");
	}

	private parseContentDivNode(contentDivNode: Element): Map<string, string> {
		for (let childNode of contentDivNode.childNodes) {
			if (childNode.nodeName === "section") {
				return this.parseSectionNode(childNode);
			}
		}
		throw new InsightError("Cannot find section node");
	}

	private parseSectionNode(sectionNode: Element): Map<string, string> {
		for (let childNode of sectionNode.childNodes) {
			if (childNode.nodeName === "div") {
				for (let childChildNode of childNode.childNodes) {
					if (childChildNode.nodeName === "div") {
						for (let attr of childChildNode.attrs) {
							if (attr.name === "class" && attr.value === "view-content") {
								return this.parseViewContentNode(childChildNode);
							}
						}
					}
				}
				throw new InsightError("Cannot find view-content");
			}
		}
		throw new InsightError("Cannot find div");
	}

	private parseViewContentNode(vcNode: Element): Map<string, string> {
		for (let childNode of vcNode.childNodes) {
			if (childNode.nodeName === "table") {
				return this.parseTableNode(childNode);
			}
		}
		throw new InsightError("Cannot find table");
	}

	private parseTableNode(tableNode: Element): Map<string, string> {
		for (let childNode of tableNode.childNodes) {
			if (childNode.nodeName === "tbody") {
				return this.parseTbodyNode(childNode);
			}
		}
		throw new InsightError("Cannot find tbody");
	}

	private parseTbodyNode(tbodyNode: Element): Map<string, string> {
		let result: Map<string, string> = new Map<string, string>();
		for (let childNode of tbodyNode.childNodes) {
			if (childNode.nodeName === "tr") {
				const arr = this.parseTrNode(childNode);
				result.set(arr[0], arr[1]);
			}
		}
		return result;
	}

	private parseTrNode(trNode: Element): string[] {
		let arr: string[] = ["", ""];
		for (let childNode of trNode.childNodes) {
			if (childNode.nodeName === "td") {
				for (let attr of childNode.attrs) {
					if (attr.name === "class" && attr.value === "views-field views-field-field-building-code") {
						for (let childChildNode of childNode.childNodes) {
							if (childChildNode.nodeName === "#text") {
								arr[1] = (childChildNode as TextNode).value.trim();
							}
						}
					}
					if (attr.name === "class" && attr.value === "views-field views-field-title") {
						for (let childChildNode of childNode.childNodes) {
							if (childChildNode.nodeName === "a") {
								for (let childChildChildNode of childChildNode.childNodes) {
									if (childChildChildNode.nodeName === "#text") {
										arr[0] = (childChildChildNode as TextNode).value.trim();
									}
								}
							}
						}
					}
				}
			}
		}
		return arr;
	}
}
