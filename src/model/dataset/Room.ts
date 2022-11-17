import http from "http";
import {InsightError} from "../../controller/IInsightFacade";

export interface GeoResponse {
	lat?: number;
	lon?: number;
	error?: string;
}

export class Room {
	private _fullname: string;
	private _shortname: string;
	private _number: string;
	private _name: string;
	private _address: string;
	private _lat: number;
	private _lon: number;
	private _seats: number;
	private _type: string;
	private _furniture: string;
	private _href: string;

	constructor() {
		this._fullname = "";
		this._shortname = "";
		this._number = "";
		this._name = "";
		this._address = "";
		this._lat = 0;
		this._lon = 0;
		this._seats = 0;
		this._type = "";
		this._furniture = "";
		this._href = "";
	}

	public get fullname(): string {
		return this._fullname;
	}

	public setFullname(value: string) {
		this._fullname = value;
	}

	public get shortname(): string {
		return this._shortname;
	}

	public setShortname(value: string) {
		this._shortname = value;
	}

	public get number(): string {
		return this._number;
	}

	public setNumber(value: string) {
		this._number = value;
	}

	public get name(): string {
		return this._name;
	}

	public setName(value: string) {
		this._name = value;
	}

	public get address(): string {
		return this._address;
	}

	public setAddress(value: string) {
		this._address = value;
	}

	public get lat(): number {
		return this._lat;
	}

	public setLat(value: number) {
		this._lat = value;
	}

	public get lon(): number {
		return this._lon;
	}

	public setLon(value: number) {
		this._lon = value;
	}

	public get seats(): number {
		return this._seats;
	}

	public setSeats(value: number) {
		this._seats = value;
	}

	public get type(): string {
		return this._type;
	}

	public setType(value: string) {
		this._type = value;
	}

	public get furniture(): string {
		return this._furniture;
	}

	public setFurniture(value: string) {
		this._furniture = value;
	}

	public get href(): string {
		return this._href;
	}

	public setHref(value: string) {
		this._href = value;
	}

	public getProperty(property: string): string | number {
		if (property === "fullname") {
			return this._fullname;
		} else if (property === "shortname") {
			return this._shortname;
		} else if (property === "number") {
			return this._number;
		} else if (property === "name") {
			return this._name;
		} else if (property === "address") {
			return this._address;
		} else if (property === "lat") {
			return this._lat;
		} else if (property === "lon") {
			return this._lon;
		} else if (property === "seats") {
			return this._seats;
		} else if (property === "type") {
			return this._type;
		} else if (property === "furniture") {
			return this._furniture;
		} else if (property === "href") {
			return this._href;
		} else {
			throw new InsightError("Cannot retrieve field for this Room");
		}
	}

	public async setGeolocation(geolocation: GeoResponse) {
		if (geolocation.error != null) {
			throw new InsightError(geolocation.error);
		}
		if (geolocation.lon == null || geolocation.lat == null) {
			throw new InsightError(geolocation.error);
			// this block should never execute
		}
		this.setLat(geolocation.lat);
		this.setLon(geolocation.lon);
	}

	public requestGeolocation(): Promise<GeoResponse> {
		return new Promise<GeoResponse>((resolve, reject) => {
			const address = this.address.replace(/ /g, "%20");
			http.get(`http://cs310.students.cs.ubc.ca:11316/api/v1/project_team107/${address}`, (response) => {
				let data = "";

				response.on("data", (chunk) => {
					data += chunk;
				});

				response.on("end", () => {
					resolve(JSON.parse(data) as GeoResponse);
				});
			}).on("error", () => {
				reject("Failed to retrieve geolocation");
			});
		});
	}
}
