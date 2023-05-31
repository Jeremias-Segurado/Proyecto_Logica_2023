import React from 'react';
import { numberToColor } from './util';

function Block({ color, className }) {   
    return (
        <div
            className={"generatedBlock" + (className ? " " + className : "")}
            style={ color === 0 ? undefined : { backgroundColor: numberToColor(color) }}
        >
            {color === 0 ? "" : color}
        </div>
    );
}

export default Block;