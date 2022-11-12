import {Element, TextNode} from "parse5/dist/tree-adapters/default";
import {Room} from "../../model/dataset/Room";
import {InsightError} from "../IInsightFacade";

export class RoomParser {
	// may or may not fail because I'm not checking if seats/furniture/type exist first
	public parseTrNode(trNode: Element): Room {
		let room = null;
		for (let td of trNode.childNodes) {
			if (td.nodeName === "td") {
				for (let attr of td.attrs) {
					if (attr.name === "class" && attr.value === "views-field views-field-field-room-number") {
						room = this.parseRoomHref(td);
					}
					if (
						attr.name === "class" &&
						attr.value === "views-field views-field-field-room-capacity" &&
						room != null
					) {
						for (let childNode of td.childNodes) {
							if (childNode.nodeName === "#text") {
								room.setSeats(+(childNode as TextNode).value.trim());
							}
						}
					}
					if (
						attr.name === "class" &&
						attr.value === "views-field views-field-field-room-furniture" &&
						room != null
					) {
						for (let childNode of td.childNodes) {
							if (childNode.nodeName === "#text") {
								room.setFurniture((childNode as TextNode).value.trim());
							}
						}
					}
					if (
						attr.name === "class" &&
						attr.value === "views-field views-field-field-room-type" &&
						room != null
					) {
						for (let childNode of td.childNodes) {
							if (childNode.nodeName === "#text") {
								room.setType((childNode as TextNode).value.trim());
							}
						}
					}
				}
			}
		}
		if (room == null) {
			throw new InsightError("Cannot find td");
		}
		return room;
	}

	private parseRoomHref(td: Element): Room {
		let room;
		for (let childNode of td.childNodes) {
			if (childNode.nodeName === "a") {
				room = this.parseRoomNumber(childNode);
				for (let attr of childNode.attrs) {
					if (attr.name === "href") {
						room.setHref(attr.value);
						return room;
					}
				}
			}
		}
		throw new InsightError("Cannot find href");
	}

	private parseRoomNumber(aNode: Element): Room {
		const room = new Room();
		for (let childNode of aNode.childNodes) {
			if (childNode.nodeName === "#text") {
				room.setNumber((childNode as TextNode).value);
				return room;
			}
		}
		throw new InsightError("Cannot find room number");
	}
}
