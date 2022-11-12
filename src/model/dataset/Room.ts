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
		if (property === "dept") {
			return this._fullname;
		} else if (property === "id") {
			return this._shortname;
		} else if (property === "avg") {
			return this._number;
		} else if (property === "instructor") {
			return this._name;
		} else if (property === "title") {
			return this._address;
		} else if (property === "pass") {
			return this._lat;
		} else if (property === "fail") {
			return this._lon;
		} else if (property === "audit") {
			return this._seats;
		} else if (property === "uuid") {
			return this._type;
		} else if (property === "year") {
			return this._furniture;
		} else if (property === "href") {
			return this._href;
		} else {
			throw new Error();
		}
	}
}
