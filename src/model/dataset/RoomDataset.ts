import {InsightDataset, InsightDatasetKind} from "../../controller/IInsightFacade";
import {Room} from "./Room";

export class RoomDataset {
	private readonly datasetInfo: InsightDataset;
	private readonly rooms: Room[];

	constructor(id: string, numRows: number, rooms: Room[]) {
		this.datasetInfo = {
			id: id,
			kind: InsightDatasetKind.Rooms,
			numRows: numRows,
		};
		this.rooms = rooms;
	}

	public getId(): string {
		return this.datasetInfo.id;
	}

	public getInsightDataset(): InsightDataset {
		return this.datasetInfo;
	}

	public getRooms(): Room[] {
		return this.rooms;
	}
}
