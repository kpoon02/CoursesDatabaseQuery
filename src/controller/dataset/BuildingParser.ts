import {Document, Element, TextNode} from "parse5/dist/tree-adapters/default";
import {Room} from "../../model/dataset/Room";
import {InsightError} from "../IInsightFacade";
import {RoomParser} from "./RoomParser";

export class BuildingParser {
	private roomParser: RoomParser;

	constructor() {
		this.roomParser = new RoomParser();
	}

	public parseDocument(document: Document): Room[] {
		for (let childNode of document.childNodes) {
			if (childNode.nodeName === "html") {
				return this.parseHtml(childNode);
			}
		}
		throw new InsightError("Cannot find HTML");
	}

	private parseHtml(htmlNode: Element): Room[] {
		for (let childNode of htmlNode.childNodes) {
			if (childNode.nodeName === "body") {
				return this.parseBody(childNode);
			}
		}
		throw new InsightError("Cannot find body");
	}

	private parseBody(bodyNode: Element): Room[] {
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

	private parseFwcDivNode(fwcDivNode: Element): Room[] {
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

	private parseMainDivNode(mainDivNode: Element): Room[] {
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

	private parseContentDivNode(contentDivNode: Element): Room[] {
		for (let childNode of contentDivNode.childNodes) {
			if (childNode.nodeName === "section") {
				return this.parseSectionNode(childNode);
			}
		}
		throw new InsightError("Cannot find section node");
	}

	private parseSectionNode(sectionNode: Element): Room[] {
		for (let childNode of sectionNode.childNodes) {
			if (childNode.nodeName === "div") {
				return this.parseVbcDivNode(childNode);
			}
		}
		throw new InsightError("Cannot find div");
	}

	private parseVbcDivNode(vbcDivNode: Element): Room[] {
		let rooms;
		for (let childNode of vbcDivNode.childNodes) {
			if (childNode.nodeName === "div") {
				for (let attr of childNode.attrs) {
					if (attr.name === "class" && attr.value === "view-footer") {
						rooms = this.parseViewFooterDivNode(childNode);
					}
				}
			}
		}
		for (let childNode of vbcDivNode.childNodes) {
			if (childNode.nodeName === "div") {
				for (let attr of childNode.attrs) {
					if (attr.name === "class" && attr.value === "view-content" && rooms != null) {
						for (let room of rooms) {
							this.parseViewContentDivNode(childNode, room);
						}
						return rooms;
					}
				}
			}
		}
		throw new InsightError("Cannot find view-footer or view-content div");
	}

	private parseViewFooterDivNode(viewFooterDivNode: Element): Room[] {
		for (let childNode of viewFooterDivNode.childNodes) {
			if (childNode.nodeName === "div") {
				for (let childChildNode of childNode.childNodes) {
					if (childChildNode.nodeName === "div") {
						for (let attr of childChildNode.attrs) {
							if (attr.name === "class" && attr.value === "view-content") {
								return this.parseVfvcDivNode(childChildNode);
							}
						}
					}
				}
			}
		}
		throw new InsightError("Cannot find view-footer view-content div");
	}

	private parseVfvcDivNode(vfvcDivNode: Element): Room[] {
		for (let childNode of vfvcDivNode.childNodes) {
			if (childNode.nodeName === "table") {
				return this.parseTableNode(childNode);
			}
		}
		throw new InsightError("Cannot find table");
	}

	private parseTableNode(tableNode: Element): Room[] {
		for (let childNode of tableNode.childNodes) {
			if (childNode.nodeName === "tbody") {
				return this.parseTbodyNode(childNode);
			}
		}
		throw new InsightError("Cannot find tbody");
	}

	private parseTbodyNode(tbodyNode: Element): Room[] {
		const rooms: Room[] = [];
		for (let childNode of tbodyNode.childNodes) {
			if (childNode.nodeName === "tr") {
				rooms.push(this.roomParser.parseTrNode(childNode));
			}
		}
		if (rooms.length === 0) {
			throw new InsightError("Cannot find tr");
		}
		return rooms;
	}

	private parseViewContentDivNode(viewContentDivNode: Element, room: Room): Room {
		for (let childNode of viewContentDivNode.childNodes) {
			if (childNode.nodeName === "div") {
				for (let childChildNode of childNode.childNodes) {
					if (childChildNode.nodeName === "div") {
						for (let attr of childChildNode.attrs) {
							if (attr.name === "id" && attr.value === "buildings-wrapper") {
								return this.parseBuildingsWrapperDivNode(childChildNode, room);
							}
						}
					}
				}
			}
		}
		throw new InsightError("Cannot find buildings wrapper");
	}

	private parseBuildingsWrapperDivNode(bwDivNode: Element, room: Room): Room {
		for (let childNode of bwDivNode.childNodes) {
			if (childNode.nodeName === "div") {
				for (let attr of childNode.attrs) {
					if (attr.name === "id" && attr.value === "building-info") {
						return this.parseBuildingInfoDivNode(childNode, room);
					}
				}
			}
		}
		throw new InsightError("Cannot find building info");
	}

	private parseBuildingInfoDivNode(biDivNode: Element, room: Room): Room {
		for (let childNode of biDivNode.childNodes) {
			if (childNode.nodeName === "h2") {
				room = this.parseRoomFullName(childNode, room);
			}
			if (childNode.nodeName === "div" && room != null) {
				room = this.parseRoomAddress(childNode, room);
			}
		}
		return room;
	}

	private parseRoomFullName(h2node: Element, room: Room): Room {
		for (let childNode of h2node.childNodes) {
			if (childNode.nodeName === "span") {
				for (let childChildNode of childNode.childNodes) {
					if (childChildNode.nodeName === "#text") {
						room.setFullname((childChildNode as TextNode).value);
						return room;
					}
				}
			}
		}
		throw new InsightError("Cannot find the room's full name");
	}

	private parseRoomAddress(childNode: Element, room: Room): Room {
		for (let childChildNode of childNode.childNodes) {
			if (room.address !== "") {
				return room;
			}
			if (childChildNode.nodeName === "div") {
				for (let childChildChildNode of childChildNode.childNodes) {
					if (childChildChildNode.nodeName === "#text") {
						room.setAddress((childChildChildNode as TextNode).value);
						return room;
					}
				}
			}
		}
		throw new InsightError("Cannot find address");
	}
}
