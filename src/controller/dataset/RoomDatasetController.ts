import {RoomDataset} from "../../model/dataset/RoomDataset";
import {InsightDataset, InsightError} from "../IInsightFacade";
import JSZip from "jszip";
import {Room} from "../../model/dataset/Room";
import {parse} from "parse5";
import {BuildingParser} from "./BuildingParser";
import {IndexParser} from "./IndexParser";
import {DatasetController} from "./DatasetController";
import * as fs from "fs-extra";

interface JsonDataset {
	datasetInfo: InsightDataset;
	rooms: JsonRoom[];
}

interface JsonRoom {
	fullname: string;
	shortname: string;
	number: string;
	name: string;
	address: string;
	lat: number;
	lon: number;
	seats: number;
	type: string;
	furniture: string;
	href: string;
}

const DIR = "./data/room";

export class RoomDatasetController {
	private readonly datasets: RoomDataset[];
	private buildingParser: BuildingParser;
	private indexParser: IndexParser;

	constructor() {
		console.log("RoomDatasetController::init()");
		this.datasets = [];
		this.buildingParser = new BuildingParser();
		this.indexParser = new IndexParser();
	}

	public loadDatasetsFromDisk() {
		if (fs.existsSync(DIR)) {
			const files = fs.readdirSync(DIR);
			for (const file of files) {
				const content = JSON.parse(fs.readFileSync(DIR + "/" + file).toString());
				this.datasets.push(this.parseDatasetFromDisk(content));
			}
		}
	}

	public async addDataset(id: string, content: string) {
		let rooms: Room[] = [];
		let promises: Array<Promise<string>> = [];
		let indexList: Map<string, string>;

		let jszip: JSZip | null = await JSZip.loadAsync(content, {base64: true});
		// throw InsightError if the zip file does not have an 'index.htm' file or
		// 'campus/discover/buildings-and-classrooms' directory, otherwise load each file
		const index = jszip.file("index.htm");
		if (index == null) {
			throw new InsightError("The root folder of the dataset zip file does not contain an index.htm file");
		} else {
			const indexString = parse(await index.async("string"));
			indexList = this.indexParser.parseDocument(indexString);
		}
		jszip = jszip.folder("campus/discover/buildings-and-classrooms");
		if (jszip == null) {
			throw new InsightError(
				"The dataset zip file does not contain a campus/discover/buildings-and-classrooms " + "directory"
			);
		}
		jszip.forEach((relativePath, file) => {
			promises.push(file.async("string"));
		});

		await Promise.all(promises)
			.then((results) => {
				for (let room of results) {
					const document = parse(room);
					let roomsToAdd = [];
					try {
						roomsToAdd = this.buildingParser.parseDocument(document);
						for (let rta of roomsToAdd) {
							const shortname = indexList.get(rta.fullname);
							if (shortname != null) {
								rta.setShortname(shortname);
								rta.setName(shortname + "_" + rta.number);
								rooms.push(rta);
							}
						}
					} catch (e) {
						// ignore the room
					}
				}
			})
			.catch((err) => {
				throw new InsightError(err);
			});

		const roomDataset = new RoomDataset(id, rooms.length, rooms);
		this.datasets.push(roomDataset);
		DatasetController.saveToDisk(roomDataset, id, DIR);
	}

	// parses the json object and returns a RoomDataset
	private parseDatasetFromDisk(json: object): RoomDataset {
		const rooms: Room[] = [];
		const results = json as unknown as JsonDataset;
		const datasetInfo = results.datasetInfo;
		for (let room of results.rooms) {
			const nRoom = new Room();
			nRoom.setFullname(room.fullname);
			nRoom.setShortname(room.shortname);
			nRoom.setNumber(room.number);
			nRoom.setName(room.name);
			nRoom.setAddress(room.address);
			nRoom.setLat(room.lat);
			nRoom.setLon(room.lon);
			nRoom.setSeats(room.seats);
			nRoom.setType(room.type);
			nRoom.setFurniture(room.furniture);
			nRoom.setHref(room.href);
			rooms.push(nRoom);
		}
		return new RoomDataset(datasetInfo.id, datasetInfo.numRows, rooms);
	}

	public getDatasets(): RoomDataset[] {
		return this.datasets;
	}
}
