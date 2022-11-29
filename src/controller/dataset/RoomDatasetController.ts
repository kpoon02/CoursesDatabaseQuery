import {RoomDataset} from "../../model/dataset/RoomDataset";
import {InsightDataset, InsightError} from "../IInsightFacade";
import JSZip from "jszip";
import {GeoResponse, Room} from "../../model/dataset/Room";
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
	_fullname: string;
	_shortname: string;
	_number: string;
	_name: string;
	_address: string;
	_lat: number;
	_lon: number;
	_seats: number;
	_type: string;
	_furniture: string;
	_href: string;
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

		await this.setGeolocations(rooms);
		const roomDataset = new RoomDataset(id, rooms.length, rooms);
		this.datasets.push(roomDataset);
		DatasetController.saveToDisk(roomDataset, id, DIR);
	}

	private async setGeolocations(rooms: Room[]) {
		let geo: Array<Promise<GeoResponse>> = [];
		for (let room of rooms) {
			geo.push(room.requestGeolocation());
		}

		let counter = 0;
		await Promise.all(geo)
			.then((results) => {
				for (let geolocation of results) {
					rooms[counter].setGeolocation(geolocation);
					counter++;
				}
			})
			.catch((err) => {
				throw new InsightError(err);
			});
	}

	// parses the json object and returns a RoomDataset
	private parseDatasetFromDisk(json: object): RoomDataset {
		const rooms: Room[] = [];
		const results = json as unknown as JsonDataset;
		const datasetInfo = results.datasetInfo as InsightDataset;
		for (let room of results.rooms) {
			const nRoom = new Room();
			nRoom.setFullname(room._fullname);
			nRoom.setShortname(room._shortname);
			nRoom.setNumber(room._number);
			nRoom.setName(room._name);
			nRoom.setAddress(room._address);
			nRoom.setLat(room._lat);
			nRoom.setLon(room._lon);
			nRoom.setSeats(room._seats);
			nRoom.setType(room._type);
			nRoom.setFurniture(room._furniture);
			nRoom.setHref(room._href);
			rooms.push(nRoom);
		}
		return new RoomDataset(datasetInfo.id, datasetInfo.numRows, rooms);
	}

	public getDatasets(): RoomDataset[] {
		return this.datasets;
	}
}
