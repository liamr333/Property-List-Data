CREATE DATABASE PROP_LISTS;

USE PROP_LISTS;

CREATE TABLE Analyst (
    Analyst_Name VARCHAR(255) NOT NULL,
    PRIMARY KEY (Analyst_Name)
);

CREATE TABLE Prop_List (
    Prop_List_Name VARCHAR(255) NOT NULL,
    Prop_List_Creator VARCHAR(255) NOT NULL,
    Prop_List_Description VARCHAR(255),
    PRIMARY KEY (Prop_List_Name),
    FOREIGN KEY (Prop_List_Creator) REFERENCES Analyst (Analyst_Name)
);


CREATE TABLE Property (
    Prop_ID VARCHAR (40) NOT NULL,
    Prop_County VARCHAR(40) NOT NULL,
    CONSTRAINT Cross_County_Prop_ID PRIMARY KEY (Prop_ID, Prop_County)
);

CREATE TABLE Entry (
    Entry_List_Name VARCHAR(255) NOT NULL,
    Entry_Prop_ID VARCHAR(255) NOT NULL,
    CONSTRAINT Entry_ID PRIMARY KEY (Entry_List_Name, Entry_Prop_ID),
    FOREIGN KEY (Entry_List_Name) REFERENCES Prop_List (Prop_List_Name),
    FOREIGN KEY (Entry_Prop_ID) REFERENCES Property (Prop_ID)
);