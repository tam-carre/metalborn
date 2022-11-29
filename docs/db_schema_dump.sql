--
-- PostgreSQL database dump
--

-- Dumped from database version 14.5
-- Dumped by pg_dump version 14.6 (Ubuntu 14.6-1.pgdg22.04+1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: description_block; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public.description_block AS ENUM (
    'AllomancyBlock',
    'FeruchemyBlock',
    'TwinbornBlock',
    'SpikesBlock',
    'MedallionBlock',
    'GrenadeBlock'
);


ALTER TYPE public.description_block OWNER TO postgres;

--
-- Name: ferring; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public.ferring AS ENUM (
    'Skimmer',
    'Steelrunner',
    'Sparker',
    'Firesoul',
    'Windwhisperer',
    'Brute',
    'Archivist',
    'Sentry',
    'Spinner',
    'Soulbearer',
    'Gasper',
    'Subsumer',
    'Trueself',
    'Connector',
    'Bloodmaker',
    'Pinnacle'
);


ALTER TYPE public.ferring OWNER TO postgres;

--
-- Name: gender; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public.gender AS ENUM (
    'Male',
    'Female',
    'Other'
);


ALTER TYPE public.gender OWNER TO postgres;

--
-- Name: halfborn; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public.halfborn AS ENUM (
    'Mistborn',
    'Feruchemist'
);


ALTER TYPE public.halfborn OWNER TO postgres;

--
-- Name: metal; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public.metal AS ENUM (
    'Iron',
    'Steel',
    'Tin',
    'Pewter',
    'Zinc',
    'Brass',
    'Copper',
    'Bronze',
    'Cadmium',
    'Bendalloy',
    'Gold',
    'Electrum',
    'Chromium',
    'Nicrosil',
    'Aluminum',
    'Duralumin'
);


ALTER TYPE public.metal OWNER TO postgres;

--
-- Name: misting; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public.misting AS ENUM (
    'Coinshot',
    'Lurcher',
    'Rioter',
    'Soother',
    'Thug',
    'Tineye',
    'Smoker',
    'Seeker',
    'DuraluminGnat',
    'AluminumGnat',
    'Augur',
    'Oracle',
    'Nicroburst',
    'Leecher',
    'Pulser',
    'Slider'
);


ALTER TYPE public.misting OWNER TO postgres;

--
-- Name: twinborn; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public.twinborn AS ENUM (
    'EagleEye',
    'Catcher',
    'Monitor',
    'Quickwit',
    'Keeneye',
    'Hefter',
    'Sprinter',
    'Sooner',
    'Scrapper',
    'Bruteblood',
    'Marathoner',
    'Scaler',
    'Deader',
    'Guardian',
    'Navigator',
    'Stalwart',
    'Sharpshooter',
    'Crasher',
    'Swift',
    'Shroud',
    'Bigshot',
    'Luckshot',
    'Cloudtoucher',
    'Copperkeep',
    'Boiler',
    'Ghostwalker',
    'Shelter',
    'Masker',
    'Sentinel',
    'Hazedodger',
    'Metalmapper',
    'Sleepless',
    'Pulsewise',
    'Stalker',
    'Strongarm',
    'Mastermind',
    'Loudmouth',
    'Zealot',
    'Highroller',
    'Instigator',
    'Schemer',
    'Cooler',
    'Icon',
    'Pacifier',
    'Slick',
    'Resolute',
    'Puremind',
    'Friendly',
    'Metalbreaker',
    'Ringer',
    'Sapper',
    'Gulper',
    'Booster',
    'BurstTicker',
    'Enabler',
    'Soulburst',
    'Cohort',
    'Chronicler',
    'Vessel',
    'Timeless',
    'Introspect',
    'Whimflitter',
    'Foresight',
    'Flicker',
    'Charmed',
    'Visionary',
    'Plotter',
    'Yearspanner',
    'Chrysalis',
    'Spotter',
    'Blur',
    'Assessor',
    'Flashwit',
    'Monument',
    'Constant',
    'Transcendent',
    'Sated'
);


ALTER TYPE public.twinborn OWNER TO postgres;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: characters; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.characters (
    name character varying NOT NULL,
    gender public.gender NOT NULL,
    fullborn boolean NOT NULL,
    halfborn public.halfborn,
    twinborn public.twinborn,
    misting public.misting,
    ferring public.ferring,
    spiked_a public.metal[] NOT NULL,
    spiked_f public.metal[] NOT NULL,
    medall_f public.metal[] NOT NULL,
    grenade boolean NOT NULL,
    CONSTRAINT ck_noinvalidmetalborn CHECK (((fullborn AND (halfborn IS NULL) AND (twinborn IS NULL) AND (misting IS NULL) AND (ferring IS NULL)) OR ((NOT fullborn) AND (halfborn = 'Mistborn'::public.halfborn) AND (twinborn IS NULL) AND (misting IS NULL)) OR ((NOT fullborn) AND (halfborn = 'Feruchemist'::public.halfborn) AND (twinborn IS NULL) AND (ferring IS NULL)) OR ((NOT fullborn) AND (halfborn IS NULL) AND (ferring IS NOT NULL) AND (misting IS NOT NULL)) OR ((NOT fullborn) AND (halfborn IS NULL) AND (twinborn IS NULL) AND (ferring IS NULL) AND (misting IS NOT NULL)) OR ((NOT fullborn) AND (halfborn IS NULL) AND (twinborn IS NULL) AND (ferring IS NOT NULL) AND (misting IS NULL))))
);


ALTER TABLE public.characters OWNER TO postgres;

--
-- Name: description_blocks; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.description_blocks (
    id integer NOT NULL,
    content text NOT NULL,
    kind public.description_block,
    character_name character varying NOT NULL,
    character_gender public.gender NOT NULL
);


ALTER TABLE public.description_blocks OWNER TO postgres;

--
-- Name: description_blocks_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.description_blocks ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.description_blocks_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: characters characters_pk; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.characters
    ADD CONSTRAINT characters_pk PRIMARY KEY (name, gender);


--
-- Name: description_blocks description_blocks_fk; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.description_blocks
    ADD CONSTRAINT description_blocks_fk FOREIGN KEY (character_name, character_gender) REFERENCES public.characters(name, gender) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- PostgreSQL database dump complete
--

