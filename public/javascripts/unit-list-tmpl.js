<div>
        $#x0024;{unitType}
        $#x0024;{provinceName}
        <select>
            <option value="move">Move</option>
            <option value="hold">Hold</option>
            <option value="supportHold">Support Hold</option>
            <option value="supportMove">Support Move</option>
            {{if unitType == "F"}}
                <option value="convoy">Convoy</option>
            {{/if}}
        </select>
    </div>